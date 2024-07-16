// Copyright (c) 2023 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

pub struct LookaheadIter<'a, T>
where
    T: PartialEq,
{
    source: &'a mut dyn Iterator<Item = T>,
    buffer: RoundQueue<T>,
    lookahead_length: usize,
}

pub const MAX_LOOKAHEAD_LENGTH: usize = 6;

struct RoundQueue<T>
where
    T: PartialEq,
{
    size: usize,
    position_read: usize,
    position_write: usize,
    data: [Option<T>; MAX_LOOKAHEAD_LENGTH],
}

impl<T> RoundQueue<T>
where
    T: PartialEq,
{
    pub fn new(size: usize) -> Self {
        assert!(size < MAX_LOOKAHEAD_LENGTH);

        Self {
            size,
            position_read: 0,
            position_write: 0,
            data: [None, None, None, None, None, None],
        }
    }

    pub fn enqueue(&mut self, value: Option<T>) {
        self.data[self.position_write] = value;

        self.position_write += 1;
        if self.position_write == self.size {
            self.position_write = 0;
        }
    }

    pub fn dequeue(&mut self) -> Option<T> {
        let value = self.data[self.position_read].take();

        self.position_read += 1;
        if self.position_read == self.size {
            self.position_read = 0;
        }

        value
    }

    pub fn peek(&self, offset: usize) -> Option<&T> {
        assert!(offset < self.size);

        let mut position = self.position_read + offset;
        if position >= self.size {
            position -= self.size;
        }

        self.data[position].as_ref()
    }
}

impl<'a, T> LookaheadIter<'a, T>
where
    T: PartialEq,
{
    pub fn new(source: &'a mut dyn Iterator<Item = T>, lookahead_length: usize) -> Self {
        let mut buffer = RoundQueue::new(lookahead_length);

        // pre-fill
        for _ in 0..lookahead_length {
            let value = source.next();
            buffer.enqueue(value);
        }

        Self {
            source,
            buffer,
            lookahead_length,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<T> {
        // take the data from buffer first, and then insert the new data,
        // because the buffer is pre-filled when it is created.
        let value = self.buffer.dequeue();

        let next_value = self.source.next();
        self.buffer.enqueue(next_value);

        value
    }

    pub fn peek(&self, offset: usize) -> Option<&T> {
        assert!(offset < self.lookahead_length);
        self.buffer.peek(offset)
    }

    pub fn equals(&self, offset: usize, expect: &T) -> bool {
        assert!(offset < self.lookahead_length);
        let value = self.peek(offset);
        matches!(value, Some(actual) if actual == expect)
    }
}

#[cfg(test)]
mod tests {
    use crate::lookaheaditer::LookaheadIter;

    #[test]
    fn test_lookahead_iter() {
        let s = "0123";
        let mut cs = s.chars();
        let mut lookahead = LookaheadIter::new(&mut cs, 3);

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
