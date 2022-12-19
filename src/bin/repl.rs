use std::error::Error;
use std::io;
use unicode_width::UnicodeWidthStr;

use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Frame, Terminal,
};

use donut::app::interpreter::Interpreter;

struct Repl {
    input: String,
    partial: String,
    command_history: Vec<String>,
    last_eval: String,
    interpreter: Interpreter,
    outputs: Vec<String>,
}

impl Repl {
    fn new() -> Repl {
        Repl {
            input: String::new(),
            partial: String::new(),
            command_history: Vec::new(),
            last_eval: String::new(),
            interpreter: Interpreter::new(),
            outputs: Vec::new(),
        }
    }

    fn evaluate(&mut self) {
        let mut input: String = self.input.drain(..).collect();
        self.command_history.push(input.clone());
        self.last_eval.clear();
        let full_command: String = self.partial.drain(..).chain(input.drain(..)).collect();
        self.last_eval
            .push_str(format!("{:?}", self.interpreter.run(&full_command)).as_str());

        self.outputs.append(&mut self.interpreter.get_output());
    }

    fn push_unevaluated(&mut self) {
        self.partial
            .push_str(&self.input.drain(..).collect::<String>());
        self.command_history.push(self.partial.clone());
        self.last_eval.clear();
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let repl = Repl::new();
    let res = run(&mut terminal, repl);

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen,)?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{:?}", err)
    }

    Ok(())
}

fn run<B: Backend>(terminal: &mut Terminal<B>, mut repl: Repl) -> io::Result<()> {
    loop {
        terminal.draw(|f| ui(f, &repl))?;

        if let Event::Key(key) = event::read()? {
            match (key.code, key.modifiers) {
                (KeyCode::Enter, KeyModifiers::NONE) => {
                    repl.evaluate();
                }
                (KeyCode::Char('n'), KeyModifiers::CONTROL) => {
                    repl.push_unevaluated();
                }
                (KeyCode::Esc, _) => {
                    return Ok(());
                }
                (KeyCode::Char(c), _) => {
                    repl.input.push(c);
                }
                (KeyCode::Backspace, _) => {
                    repl.input.pop();
                }
                _ => (),
            }
        }
    }
}

fn ui<B: Backend>(frame: &mut Frame<B>, repl: &Repl) {
    let halves = Layout::default()
        .direction(Direction::Horizontal)
        .margin(0)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
        .split(frame.size());

    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        .constraints(
            [
                Constraint::Length(2),
                Constraint::Length(3),
                Constraint::Ratio(2, 3),
                Constraint::Length(2),
            ]
            .as_ref(),
        )
        .split(halves[0]);

    let output_chunk = Layout::default()
        .margin(1)
        .constraints([Constraint::Percentage(100)].as_ref())
        .split(halves[1]);

    let evaluation_message = vec![
        Span::raw("Press "),
        Span::styled("Enter", Style::default().add_modifier(Modifier::BOLD)),
        Span::raw(" to evaluate input, or "),
        Span::styled("Ctrl+n", Style::default().add_modifier(Modifier::BOLD)),
        Span::raw(" to continue typing input on the next line."),
    ];
    let exit_message = vec![
        Span::raw(" Press "),
        Span::styled("Esc", Style::default().add_modifier(Modifier::BOLD)),
        Span::raw(" to exit."),
    ];
    let text = Text::from(vec![
        Spans::from(evaluation_message),
        Spans::from(exit_message),
    ]);
    let help_message = Paragraph::new(text).wrap(tui::widgets::Wrap { trim: true });
    frame.render_widget(help_message, chunks[0]);

    let input = Paragraph::new(repl.input.as_ref())
        .block(Block::default().borders(Borders::ALL).title("Input"));
    frame.render_widget(input, chunks[1]);

    frame.set_cursor(chunks[1].x + repl.input.width() as u16 + 1, chunks[1].y + 1);

    let history: Vec<ListItem> = repl
        .command_history
        .iter()
        .enumerate()
        .map(|(i, line)| {
            let content = vec![Spans::from(Span::raw(format!("{}: {}", i, line)))];
            ListItem::new(content)
        })
        .collect();
    let history = List::new(history).block(Block::default().borders(Borders::ALL).title("History"));
    frame.render_widget(history, chunks[2]);

    let last_eval = Paragraph::new(repl.last_eval.as_ref())
        .block(Block::default().borders(Borders::ALL).title("Returned"));
    frame.render_widget(last_eval, chunks[3]);

    let outputs: Vec<ListItem> = repl
        .outputs
        .iter()
        .map(|line| {
            let content = vec![Spans::from(Span::raw(line))];
            ListItem::new(content)
        })
        .collect();
    let outputs = List::new(outputs).block(Block::default().borders(Borders::ALL).title("Output"));
    frame.render_widget(outputs, output_chunk[0]);
}
