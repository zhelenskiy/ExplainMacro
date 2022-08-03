use std::cmp::max;
use std::ops::Add;
use pad::PadStr;

#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub enum Alignment {
    Begin,
    Center,
    End,
}

fn empty_lines(n: usize) -> impl Iterator<Item=String> {
    (0..n).map(|_| String::new())
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct StringBlock {
    lines: Vec<String>,
    width: usize,
}

impl StringBlock {
    pub fn width(&self) -> usize { self.width }
    pub fn height(&self) -> usize { self.lines.len() }

    pub fn from_lines(lines: Vec<String>) -> StringBlock {
        assert!(lines.iter().all(|x| !x.contains('\n')));
        let width = lines.iter().max_by_key(|x| x.len()).map(|x| x.len()).unwrap_or_default();
        StringBlock { lines, width: width }
    }

    pub fn from_lines_iter<'a, I: Iterator<Item=&'a str>>(lines: I) -> StringBlock {
        Self::from_lines(lines.map(|x| String::from(x)).collect())
    }

    pub fn from_lines_str(lines: &[&str]) -> StringBlock {
        Self::from_lines(lines.iter().map(|x| String::from(*x)).collect())
    }

    pub fn from_str(line: &str) -> StringBlock {
        Self::from_lines_iter(line.lines())
    }

    pub fn add_to_right(self, other: StringBlock, alignment: Alignment, additional_separator: &str) -> StringBlock {
        let height = max(self.height(), other.height());
        let self_is_smaller_by = height - self.height();
        let other_is_smaller_by = height - other.height();
        let self_width = self.width();
        let left_lines: Vec<_> = match alignment {
            Alignment::Begin => self.lines.into_iter()
                .chain(empty_lines(self_is_smaller_by))
                .collect(),
            Alignment::Center => empty_lines(self_is_smaller_by / 2)
                .chain(self.lines.into_iter())
                .chain(empty_lines(self_is_smaller_by - self_is_smaller_by / 2))
                .collect(),
            Alignment::End => empty_lines(self_is_smaller_by).into_iter()
                .chain(self.lines)
                .collect(),
        };
        let right_lines: Vec<_> = match alignment {
            Alignment::Begin => other.lines.into_iter()
                .chain(empty_lines(other_is_smaller_by))
                .collect(),
            Alignment::Center => empty_lines(other_is_smaller_by / 2)
                .chain(other.lines.into_iter())
                .chain(empty_lines(other_is_smaller_by - other_is_smaller_by / 2))
                .collect(),
            Alignment::End => empty_lines(other_is_smaller_by).into_iter()
                .chain(other.lines)
                .collect(),
        };
        let line_combiner = |left: String, right: String| {
            if right.is_empty() { left } else {
                let padded_left = left.pad_to_width(self_width);
                format!("{padded_left}{additional_separator}{right}")
            }
        };
        let lines = left_lines.into_iter()
            .zip(right_lines)
            .map(|(x, y)| line_combiner(x, y))
            .collect();
        Self::from_lines(lines)
    }

    pub fn add_to_left(self, other: StringBlock, alignment: Alignment, additional_separator: &str) -> StringBlock {
        other.add_to_right(self, alignment, additional_separator)
    }

    pub fn add_to_bottom(self, other: StringBlock, alignment: Alignment) -> StringBlock {
        if other.width() == 0 || other.height() == 0 {
            return self;
        } else if self.width() == 0 || self.height() == 0 {
            return other;
        }
        let width = max(self.width(), other.width());
        let self_is_smaller_by = width - self.width();
        let other_is_smaller_by = width - other.width();
        let self_indent = " ".repeat(match alignment {
            Alignment::Begin => 0,
            Alignment::Center => self_is_smaller_by / 2,
            Alignment::End => self_is_smaller_by
        });
        let other_indent = " ".repeat(match alignment {
            Alignment::Begin => 0,
            Alignment::Center => other_is_smaller_by / 2,
            Alignment::End => other_is_smaller_by
        });
        let lines = self.lines.into_iter()
            .map(|s| format!("{self_indent}{s}"))
            .chain(other.lines.into_iter().map(|s| format!("{other_indent}{s}")))
            .collect();
        StringBlock { lines, width }
    }

    pub fn add_to_top(self, other: StringBlock, alignment: Alignment) -> StringBlock {
        other.add_to_bottom(self, alignment)
    }

    pub fn make_string(&self) -> String {
        self.lines.join("\n")
    }
}

impl Add for StringBlock {
    type Output = StringBlock;
    fn add(self, rhs: Self) -> Self::Output {
        self.add_to_right(rhs, Alignment::Begin, "")
    }
}

impl Add<&str> for StringBlock {
    type Output = StringBlock;
    fn add(self, rhs: &str) -> Self::Output {
        self + StringBlock::from_str(rhs)
    }
}

impl Add<StringBlock> for &str {
    type Output = StringBlock;
    fn add(self, rhs: StringBlock) -> Self::Output {
        StringBlock::from_str(self) + rhs
    }
}

pub fn join(iter: impl Iterator<Item=StringBlock>) -> StringBlock {
    iter.fold(StringBlock::from_str(""), StringBlock::add)
}

#[cfg(test)]
mod tests {
    use crate::render::Alignment::{Begin, Center, End};
    use crate::render::{Alignment, StringBlock};

    #[derive(Clone)]
    struct Data {
        small: StringBlock,
        medium: StringBlock,
        big: StringBlock,
    }

    fn make_data() -> Data {
        Data {
            small: StringBlock::from_str("*"),
            medium: StringBlock::from_lines_str(&["#1", "#2"]),
            big: StringBlock::from_lines_str(&["@", "@@", "@@@"]),
        }
    }

    #[test]
    fn to_string() {
        let data = make_data();
        assert_eq!(data.big.make_string(), "@\n@@\n@@@");
        assert_eq!(data.medium.make_string(), "#1\n#2");
        assert_eq!(data.small.make_string(), "*");
    }


    #[test]
    fn right() {
        let data = make_data();
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_right(data.small, Begin, "|").make_string(),
                "@  |*\n@@\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_right(data.small, Center, "|").make_string(),
                "@\n@@ |*\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_right(data.small, End, "|").make_string(),
                "@\n@@\n@@@|*"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_right(data.medium, Begin, "|").make_string(),
                "@  |#1\n@@ |#2\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_right(data.medium, Center, "|").make_string(),
                "@  |#1\n@@ |#2\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_right(data.medium, End, "|").make_string(),
                "@\n@@ |#1\n@@@|#2"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.small.add_to_right(data.big, Begin, "|").make_string(),
                "*|@\n |@@\n |@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.small.add_to_right(data.big, Center, "|").make_string(),
                " |@\n*|@@\n |@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.small.add_to_right(data.big, End, "|").make_string(),
                " |@\n |@@\n*|@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.medium.add_to_right(data.big, Begin, "|").make_string(),
                "#1|@\n#2|@@\n  |@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.medium.add_to_right(data.big, Center, "|").make_string(),
                "#1|@\n#2|@@\n  |@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.medium.add_to_right(data.big, End, "|").make_string(),
                "  |@\n#1|@@\n#2|@@@"
            );
        }

        for alignment in [Alignment::Begin, Alignment::Center, Alignment::End] {
            let data = data.clone();
            assert_eq!(
                data.big.clone().add_to_right(data.big, alignment, "|").make_string(),
                "@  |@\n@@ |@@\n@@@|@@@"
            );
        }
    }

    #[test]
    fn left() {
        let data = make_data();
        assert_eq!(
            data.big.add_to_left(data.small, Center, "|").make_string(),
            " |@\n*|@@\n |@@@"
        );
    }


    #[test]
    fn botton() {
        let data = make_data();
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_bottom(data.small, Begin).make_string(),
                "@\n@@\n@@@\n*"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_bottom(data.small, Center).make_string(),
                "@\n@@\n@@@\n *"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_bottom(data.small, End).make_string(),
                "@\n@@\n@@@\n  *"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_bottom(data.medium, Begin).make_string(),
                "@\n@@\n@@@\n#1\n#2"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_bottom(data.medium, Center).make_string(),
                "@\n@@\n@@@\n#1\n#2"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.big.add_to_bottom(data.medium, End).make_string(),
                "@\n@@\n@@@\n #1\n #2"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.small.add_to_bottom(data.big, Begin).make_string(),
                "*\n@\n@@\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.small.add_to_bottom(data.big, Center).make_string(),
                " *\n@\n@@\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.small.add_to_bottom(data.big, End).make_string(),
                "  *\n@\n@@\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.medium.add_to_bottom(data.big, Begin).make_string(),
                "#1\n#2\n@\n@@\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.medium.add_to_bottom(data.big, Center).make_string(),
                "#1\n#2\n@\n@@\n@@@"
            );
        }
        {
            let data = data.clone();
            assert_eq!(
                data.medium.add_to_bottom(data.big, End).make_string(),
                " #1\n #2\n@\n@@\n@@@"
            );
        }

        for alignment in [Alignment::Begin, Alignment::Center, Alignment::End] {
            let data = data.clone();
            assert_eq!(
                data.big.clone().add_to_bottom(data.big, alignment).make_string(),
                "@\n@@\n@@@\n@\n@@\n@@@"
            );
        }
    }

    #[test]
    fn top() {
        let data = make_data();
        assert_eq!(
            data.big.add_to_top(data.small, Center).make_string(),
            " *\n@\n@@\n@@@"
        );
    }
}
