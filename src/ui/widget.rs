use tui::widgets::ListState;

use tui::{
    style::{Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, List, ListItem, Paragraph},
};

use unicode_width::UnicodeWidthStr;

pub struct InputBox {
    pub text: String,
    pub cursor_position: usize,
}

impl InputBox {
    pub fn new() -> Self {
        InputBox {
            text: String::default(),
            cursor_position: 0,
        }
    }

    pub fn move_cursor(&mut self, diff: isize) {
        self.cursor_position = self.cursor_position.saturating_add_signed(diff);
        let max_cursor = self.text.width();
        if self.cursor_position > max_cursor {
            self.cursor_position = max_cursor;
        }
    }

    pub fn cursor_to_start(&mut self) {
        self.cursor_position = 0;
    }

    pub fn cursor_to_end(&mut self) {
        self.cursor_position = self.text.width();
    }

    pub fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    pub fn set(&mut self, text: String) {
        self.text = text;
        self.cursor_position = self.text.width();
    }

    pub fn insert(&mut self, new_char: char) {
        self.text.insert(self.cursor_position, new_char);
        self.cursor_position = self.cursor_position.saturating_add(1);
    }

    pub fn remove(&mut self) {
        if self.cursor_position == 0 {
            return;
        }
        self.text.remove(self.cursor_position - 1);
        self.cursor_position = self.cursor_position.saturating_sub(1);
    }

    pub fn clear(&mut self) -> String {
        self.cursor_position = 0;
        self.text.drain(..).collect()
    }
}

pub struct HistoryList {
    pub state: ListState,
    pub items: Vec<String>,
    pub eval_index: usize,
}

impl HistoryList {
    pub fn new() -> Self {
        HistoryList {
            state: ListState::default(),
            items: Vec::new(),
            eval_index: 0,
        }
    }

    pub fn get_new_lines_to_cursor(&mut self) -> Vec<String> {
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
        returned_items
    }

    pub fn push(&mut self, item: String) {
        if self.items.is_empty() {
            self.items.push(item);
            self.state.select(Some(0));
            return;
        }
        if self.state.selected().is_none() {
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

    pub fn pop(&mut self) -> Option<String> {
        if self.items.is_empty() {
            return None;
        }
        if self.eval_index >= self.items.len() {
            return None;
        }
        if self.state.selected().is_none() {
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
        None
    }

    pub fn next(&mut self) {
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

    pub fn previous(&mut self) {
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

    pub fn get_selected(&mut self) -> Option<&String> {
        if let Some(selected) = self.state.selected() {
            return self.items.get(selected);
        }
        None
    }
}

pub fn build_message_widget<'a>() -> Paragraph<'a> {
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

pub fn build_paragraph_widget<'a>(input: &'a str, title: &'a str) -> Paragraph<'a> {
    return Paragraph::new(input)
        .block(Block::default().borders(Borders::ALL).title(title))
        .wrap(tui::widgets::Wrap { trim: true });
}

pub fn build_list_widget<'a>(
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
                .map(Spans::from)
                .collect::<Vec<Spans>>()
        })
        .flat_map(|v| v.into_iter())
        .enumerate()
        .map(|(i, spans)| {
            if separator_index.is_none() {
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
