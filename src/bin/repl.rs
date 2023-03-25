use crossterm::terminal::SetTitle;
use donut::ui::widget::{
    build_list_widget, build_message_widget, build_paragraph_widget, HistoryList, InputBox,
};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::{error::Error, io};

use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    Frame, Terminal,
};

use donut::app::interpreter::Interpreter;

struct Repl {
    input: InputBox,
    command_history: HistoryList,
    last_eval: String,
    interpreter: Interpreter,
    outputs: HistoryList,
}

impl Repl {
    fn new() -> Repl {
        Repl {
            input: InputBox::new(),
            command_history: HistoryList::new(),
            last_eval: String::new(),
            interpreter: Interpreter::new(),
            outputs: HistoryList::new(),
        }
    }

    fn evaluate(&mut self) {
        let input = self.input.clear();

        if !input.is_empty() {
            self.command_history.push(input);
        }

        let commands_to_evaluate = self.command_history.get_new_lines_to_cursor();
        if commands_to_evaluate.is_empty() {
            return;
        }

        self.last_eval.clear();
        let full_command: String = commands_to_evaluate
            .into_iter()
            .collect::<Vec<String>>()
            .join(" ");

        self.last_eval
            .push_str(format!("{:?}", self.interpreter.run(&full_command)).as_str());

        for line in self.interpreter.get_output() {
            self.outputs.push(line);
        }
    }

    fn push_unevaluated(&mut self) {
        let additions = self.input.clear();
        self.command_history.push(additions);
        self.last_eval.clear();
    }

    fn pop_unevaluated(&mut self) {
        if let Some(popped) = self.command_history.pop() {
            self.input.set(popped);
        }
        self.last_eval.clear();
    }

    fn delete_unevaluated(&mut self) {
        self.command_history.pop();
        self.last_eval.clear();
    }

    fn evaluate_file(&mut self) {
        if self.input.is_empty() {
            return;
        }
        let filename = self.input.clear();
        if let Ok(file) = File::open(filename.clone()) {
            for line in BufReader::new(file).lines() {
                self.input.set(line.ok().unwrap_or_default());
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
                        repl.input.set(command.clone());
                    } else {
                        repl.input.clear();
                    }
                }
                (KeyCode::Char('x'), KeyModifiers::CONTROL) => {
                    repl.pop_unevaluated();
                }
                (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                    repl.delete_unevaluated();
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
                (KeyCode::Left, KeyModifiers::NONE) => {
                    repl.input.move_cursor(-1);
                }
                (KeyCode::Right, KeyModifiers::NONE) => {
                    repl.input.move_cursor(1);
                }
                (KeyCode::Home, KeyModifiers::NONE) => {
                    repl.input.cursor_to_start();
                }
                (KeyCode::End, KeyModifiers::NONE) => {
                    repl.input.cursor_to_end();
                }
                (KeyCode::Esc, _) => {
                    return Ok(());
                }
                (KeyCode::Char(c), _) => {
                    repl.input.insert(c);
                }
                (KeyCode::Backspace, _) => {
                    repl.input.remove();
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

    frame.render_widget(build_paragraph_widget(&repl.input.text, "Input"), chunks[1]);
    frame.set_cursor(
        chunks[1].x + repl.input.cursor_position as u16 + 1,
        chunks[1].y + 1,
    );

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
