use crossterm::terminal::SetTitle;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::{error::Error, io};
use tui::widgets::ListState;
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

struct HistoryList {
    state: ListState,
    items: Vec<String>,
    eval_index: usize,
}

impl HistoryList {
    fn new() -> HistoryList {
        HistoryList {
            state: ListState::default(),
            items: Vec::new(),
            eval_index: 0,
        }
    }

    fn get_new_lines_to_cursor(&mut self) -> Vec<String> {
        let stop_index: usize;
        if let Some(selected) = self.state.selected() {
            stop_index = selected + 1;
        } else {
            stop_index = self.items.len();
        }
        let mut returned_items: Vec<String> = Vec::new();
        if let Some(range) = self.items.get(self.eval_index..stop_index) {
            for item in range {
                returned_items.push(item.clone());
            }
        }
        self.eval_index = stop_index;
        return returned_items;
    }

    fn push(&mut self, item: String) {
        if self.items.len() == 0 {
            self.items.push(item);
            self.state.select(Some(0));
            return;
        }
        if let None = self.state.selected() {
            self.items.push(item);
            self.state.select(Some(self.items.len() - 1));
            return;
        }
        let selected = self.state.selected().unwrap();
        if selected >= self.eval_index.saturating_sub(1) {
            self.items.insert(selected + 1, item);
            self.state.select(Some(selected + 1));
        } else {
            self.items.push(item);
            self.state.select(Some(self.items.len() - 1));
        }
    }

    fn pop(&mut self) -> Option<String> {
        if self.items.len() == 0 {
            return None;
        }
        if self.eval_index >= self.items.len() {
            return None;
        }
        if let None = self.state.selected() {
            return self.items.pop();
        }
        let selected = self.state.selected().unwrap();
        if self.eval_index == 0 {
            let item = self.items.remove(selected);
            self.state.select(Some(selected.saturating_sub(1)));
            return Some(item);
        }
        if selected > self.eval_index.saturating_sub(1) {
            let item = self.items.remove(selected);
            self.state.select(Some(selected.saturating_sub(1)));
            return Some(item);
        }
        return None;
    }

    fn next(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i >= self.items.len() - 1 {
                    0
                } else {
                    i + 1
                }
            }
            None => 0,
        };
        self.state.select(Some(i));
    }

    fn previous(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i == 0 {
                    self.items.len() - 1
                } else {
                    i - 1
                }
            }
            None => 0,
        };
        self.state.select(Some(i));
    }

    fn get_selected(&mut self) -> Option<&String> {
        if let Some(selected) = self.state.selected() {
            return self.items.get(selected);
        }
        return None;
    }
}

struct Repl {
    input: String,
    command_history: HistoryList,
    last_eval: String,
    interpreter: Interpreter,
    outputs: HistoryList,
}

impl Repl {
    fn new() -> Repl {
        Repl {
            input: String::new(),
            command_history: HistoryList::new(),
            last_eval: String::new(),
            interpreter: Interpreter::new(),
            outputs: HistoryList::new(),
        }
    }

    fn evaluate(&mut self) {
        let input: String = self.input.drain(..).collect();

        if input.len() > 0 {
            self.command_history.push(input);
        }

        let commands_to_evaluate = self.command_history.get_new_lines_to_cursor();
        if commands_to_evaluate.len() == 0 {
            return;
        }

        self.last_eval.clear();
        let full_command: String = commands_to_evaluate
            .into_iter()
            .map(|s| s.clone())
            .collect::<Vec<String>>()
            .join(" ");

        self.last_eval
            .push_str(format!("{:?}", self.interpreter.run(&full_command)).as_str());

        for line in self.interpreter.get_output() {
            self.outputs.push(line);
        }
    }

    fn push_unevaluated(&mut self) {
        let additions = self.input.drain(..).collect::<String>();
        self.command_history.push(additions);
        self.last_eval.clear();
    }

    fn pop_unevaluated(&mut self) {
        if let Some(popped) = self.command_history.pop() {
            self.input = popped;
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
    execute!(stdout, EnterAlternateScreen, SetTitle("Dough"))?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let repl = Repl::new();
    let res = run(&mut terminal, repl);

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{:?}", err)
    }

    Ok(())
}

fn run<B: Backend>(terminal: &mut Terminal<B>, mut repl: Repl) -> io::Result<()> {
    loop {
        terminal.draw(|f| ui(f, &mut repl))?;

        if let Event::Key(key) = event::read()? {
            match (key.code, key.modifiers) {
                (KeyCode::Enter, KeyModifiers::NONE) => {
                    repl.push_unevaluated();
                }
                (KeyCode::Char('e'), KeyModifiers::CONTROL) => {
                    repl.evaluate();
                }
                (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                    if let Some(command) = repl.command_history.get_selected() {
                        repl.input = command.clone();
                    } else {
                        repl.input.clear();
                    }
                }
                (KeyCode::Char('u'), KeyModifiers::CONTROL) => {
                    repl.pop_unevaluated();
                }
                (KeyCode::Char('o'), KeyModifiers::CONTROL) => {
                    repl.evaluate_file();
                }
                (KeyCode::Up, KeyModifiers::NONE) => {
                    repl.command_history.previous();
                }
                (KeyCode::Down, KeyModifiers::NONE) => {
                    repl.command_history.next();
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

fn ui<B: Backend>(frame: &mut Frame<B>, repl: &mut Repl) {
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

    frame.render_stateful_widget(
        build_list_widget(
            &repl.command_history.items,
            Some(repl.command_history.eval_index),
            ">> ",
        ),
        chunks[2],
        &mut repl.command_history.state,
    );

    frame.render_widget(
        build_paragraph_widget(&repl.last_eval, "Returned"),
        chunks[3],
    );

    frame.render_stateful_widget(
        build_list_widget(&repl.outputs.items, None, ""),
        output_chunk[0],
        &mut repl.outputs.state,
    );
}

fn build_message_widget<'a>() -> Paragraph<'a> {
    let evaluation_message = vec![
        Span::raw("Press "),
        Span::styled("Enter", Style::default().add_modifier(Modifier::BOLD)),
        Span::raw(" to continue typing input on the next line, or "),
        Span::styled("Ctrl+e", Style::default().add_modifier(Modifier::BOLD)),
        Span::raw(" to begin evaluation."),
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
    separator_index: Option<usize>,
    select_flag: &'a str,
) -> List<'a> {
    let list: Vec<ListItem> = items
        .iter()
        .enumerate()
        .map(|(i, text)| {
            format!("{}: {}", i + 1, text)
                .lines()
                .map(|l| l.to_owned())
                .map(|l| Spans::from(l))
                .collect::<Vec<Spans>>()
        })
        .flat_map(|v| v.into_iter())
        .enumerate()
        .map(|(i, spans)| {
            if let None = separator_index {
                return ListItem::new(spans);
            }
            if i < separator_index.unwrap() {
                return ListItem::new(spans).style(Style {
                    fg: None,
                    bg: None,
                    add_modifier: Modifier::DIM,
                    sub_modifier: Modifier::empty(),
                });
            }
            return ListItem::new(spans);
        })
        .collect();

    return List::new(list)
        .block(Block::default().borders(Borders::ALL).title("History"))
        .highlight_symbol(select_flag);
}
