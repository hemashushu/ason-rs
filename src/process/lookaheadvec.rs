// Copyright (c) 2024 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

use std::collections::VecDeque;

pub struct LookaheadVec<T>
where
    T: PartialEq,
{
    source: VecDeque<T>,
}

impl<T> LookaheadVec<T>
where
    T: PartialEq,
{
    pub fn new(v: Vec<T>) -> Self {
        let source = VecDeque::from(v);
        Self { source }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<T> {
        self.source.pop_front()
    }

    pub fn peek(&self, offset: usize) -> Option<&T> {
        self.source.get(offset)
    }

    pub fn equals(&self, offset: usize, expect: &T) -> bool {
        matches!(
            self.source.get(offset),
            Some(actual) if actual == expect
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::process::lookaheadvec::LookaheadVec;

    #[test]
    fn test_lookahead_vec() {
        let s = "0123";
        let cs = s.chars().collect::<Vec<char>>();
        let mut lookahead = LookaheadVec::new(cs);

        // init
        assert_eq!(Some(&'0'), lookahead.peek(0));
        assert_eq!(Some(&'1'), lookahead.peek(1));
        assert_eq!(Some(&'2'), lookahead.peek(2));
        assert!(!lookahead.equals(0, &'1'));
        assert!(lookahead.equals(1, &'1'));
        assert!(!lookahead.equals(2, &'1'));

        // consume one item
        assert_eq!(Some('0'), lookahead.next());
        assert_eq!(Some(&'1'), lookahead.peek(0));
        assert_eq!(Some(&'2'), lookahead.peek(1));
        assert_eq!(Some(&'3'), lookahead.peek(2));
        assert!(lookahead.equals(0, &'1'));
        assert!(!lookahead.equals(1, &'1'));
        assert!(!lookahead.equals(2, &'1'));

        assert_eq!(Some('1'), lookahead.next());
        assert_eq!(Some(&'2'), lookahead.peek(0));
        assert_eq!(Some(&'3'), lookahead.peek(1));
        assert_eq!(None, lookahead.peek(2));

        assert_eq!(Some('2'), lookahead.next());
        assert_eq!(Some(&'3'), lookahead.peek(0));
        assert_eq!(None, lookahead.peek(1));
        assert_eq!(None, lookahead.peek(2));

        assert_eq!(Some('3'), lookahead.next());
        assert_eq!(None, lookahead.peek(0));
        assert_eq!(None, lookahead.peek(1));
        assert_eq!(None, lookahead.peek(2));

        assert_eq!(None, lookahead.next());
        assert_eq!(None, lookahead.peek(0));
        assert_eq!(None, lookahead.peek(1));
        assert_eq!(None, lookahead.peek(2));
    }
}
