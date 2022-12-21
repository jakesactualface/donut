use std::fs::File;
use std::io::{BufRead, BufReader};
use std::{error::Error, io};
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
    command_eval_index: usize,
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
            command_eval_index: 0,
            last_eval: String::new(),
            interpreter: Interpreter::new(),
            outputs: Vec::new(),
        }
    }

    fn evaluate(&mut self) {
        let mut input: String = self.input.drain(..).collect();
        self.command_history.push(input.clone());
        self.command_eval_index = self.command_history.len();
        self.last_eval.clear();
        let full_command: String = self.partial.drain(..).chain(input.drain(..)).collect();
        self.last_eval
            .push_str(format!("{:?}", self.interpreter.run(&full_command)).as_str());

        self.outputs.append(&mut self.interpreter.get_output());
    }

    fn push_unevaluated(&mut self) {
        let additions = self.input.drain(..).collect::<String>();
        self.partial.push_str(&additions);
        self.command_history.push(additions);
        self.last_eval.clear();
    }

    fn pop_unevaluated(&mut self) {
        if self.partial.is_empty() {
            return;
        }
        if let Some(last_partial) = self.command_history.pop() {
            self.partial
                .truncate(self.partial.len() - last_partial.len());
            self.input = last_partial;
        }
        self.last_eval.clear();
    }

    fn evaluate_file(&mut self) {
        if self.input.is_empty() {
            return;
        }
        let filename = self.input.drain(..).collect::<String>();
        if let Ok(file) = File::open(filename.clone()) {
            for line in BufReader::new(file).lines() {
                self.input = line.ok().unwrap_or_default();
                self.push_unevaluated();
            }
        } else {
            self.last_eval = format!("Error: file not found: {filename}");
        }
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
                (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                    repl.pop_unevaluated();
                }
                (KeyCode::Char('o'), KeyModifiers::CONTROL) => {
                    repl.evaluate_file();
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

    frame.render_widget(build_message_widget(), chunks[0]);

    frame.render_widget(build_paragraph_widget(&repl.input, "Input"), chunks[1]);
    frame.set_cursor(chunks[1].x + repl.input.width() as u16 + 1, chunks[1].y + 1);

    frame.render_widget(
        build_list_widget(
            &repl.command_history,
            chunks[2].height - 2,
            Some(repl.command_eval_index),
        ),
        chunks[2],
    );

    frame.render_widget(
        build_paragraph_widget(&repl.last_eval, "Returned"),
        chunks[3],
    );

    frame.render_widget(
        build_list_widget(&repl.outputs, output_chunk[0].height - 3, None),
        output_chunk[0],
    );
}

fn build_message_widget<'a>() -> Paragraph<'a> {
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
    return Paragraph::new(text).wrap(tui::widgets::Wrap { trim: true });
}

fn build_paragraph_widget<'a>(input: &'a str, title: &'a str) -> Paragraph<'a> {
    return Paragraph::new(input)
        .block(Block::default().borders(Borders::ALL).title(title))
        .wrap(tui::widgets::Wrap { trim: true });
}

fn build_list_widget<'a>(
    items: &'a Vec<String>,
    max_length: u16,
    separator_index: Option<usize>,
) -> List<'a> {
    let max_length: usize = max_length.into();
    let mut list: Vec<ListItem> = items
        .iter()
        .enumerate()
        .map(|(i, text)| {
            format!("{}: {}", i, text)
                .lines()
                .map(|l| l.to_owned())
                .map(|l| Spans::from(l))
                .collect::<Vec<Spans>>()
        })
        .flat_map(|v| v.into_iter())
        .map(|spans| ListItem::new(spans))
        .collect();

    if let Some(index) = separator_index {
        list.insert(index, ListItem::new(tui::symbols::line::HORIZONTAL));
    }

    let start_index = if list.len() > max_length {
        list.len() - max_length
    } else {
        0
    };
    let truncated_list: Vec<ListItem> = list.into_iter().skip(start_index).collect();
    return List::new(truncated_list)
        .block(Block::default().borders(Borders::ALL).title("History"));
}
