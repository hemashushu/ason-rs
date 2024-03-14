// Copyright (c) 2023 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

pub struct PeekableIterator<'a, T>
where
    T: PartialEq,
{
    src: &'a mut dyn Iterator<Item = T>,
    buffer: PeekableIteratorBuffer<T>,
}

struct PeekableIteratorBuffer<T>
where
    T: PartialEq,
{
    length: usize,
    read_position: usize,
    write_position: usize,
    data: Vec<Option<T>>,
}

impl<T> PeekableIteratorBuffer<T>
where
    T: PartialEq,
{
    pub fn new(length: usize) -> Self {
        let mut data: Vec<Option<T>> = Vec::with_capacity(length);
        for _ in 0..length {
            data.push(None)
        }

        Self {
            length,
            read_position: 0,
            write_position: 0,
            data,
        }
    }

    pub fn fill(&mut self, value: Option<T>) {
        self.data[self.write_position] = value;

        self.write_position += 1;
        if self.write_position == self.length {
            self.write_position = 0;
        }
    }

    pub fn take(&mut self) -> Option<T> {
        let value = self.data[self.read_position].take();

        self.read_position += 1;
        if self.read_position == self.length {
            self.read_position = 0;
        }

        value
    }

    pub fn peek(&self, offset: usize) -> &Option<T> {
        assert!(offset < self.length);

        let mut position = self.read_position + offset;
        if position >= self.length {
            position -= self.length;
        }
        &self.data[position]
    }
}

impl<'a, T> PeekableIterator<'a, T>
where
    T: PartialEq,
{
    pub fn new(src: &'a mut dyn Iterator<Item = T>, look_ahead_length: usize) -> Self {
        let mut buffer = PeekableIteratorBuffer::new(look_ahead_length);
        for _ in 0..look_ahead_length {
            let value = src.next();
            buffer.fill(value);
        }

        Self { src, buffer }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<T> {
        // take first because the buffer is full filled when it is created.
        let value = self.buffer.take();

        let next_value = self.src.next();
        self.buffer.fill(next_value);

        value
    }

    pub fn peek(&self, offset: usize) -> &Option<T> {
        self.buffer.peek(offset)
    }

    pub fn look_ahead_equals(&self, offset: usize, expect: &T) -> bool {
        let value = self.peek(offset);
        matches!(value, Some(actual) if actual == expect)
    }
}

#[cfg(test)]
mod tests {
    use crate::peekable_iterator::PeekableIterator;

    #[test]
    fn test_peekable_iterator() {
        let s = "0123";
        let mut cs = s.chars();
        let mut pt = PeekableIterator::new(&mut cs, 3);

        // init
        assert_eq!(&Some('0'), pt.peek(0));
        assert_eq!(&Some('1'), pt.peek(1));
        assert_eq!(&Some('2'), pt.peek(2));
        assert!(!pt.look_ahead_equals(0, &'1'));
        assert!(pt.look_ahead_equals(1, &'1'));
        assert!(!pt.look_ahead_equals(2, &'1'));

        // consume one item
        assert_eq!(Some('0'), pt.next());
        assert_eq!(&Some('1'), pt.peek(0));
        assert_eq!(&Some('2'), pt.peek(1));
        assert_eq!(&Some('3'), pt.peek(2));
        assert!(pt.look_ahead_equals(0, &'1'));
        assert!(!pt.look_ahead_equals(1, &'1'));
        assert!(!pt.look_ahead_equals(2, &'1'));

        assert_eq!(Some('1'), pt.next());
        assert_eq!(&Some('2'), pt.peek(0));
        assert_eq!(&Some('3'), pt.peek(1));
        assert_eq!(&None, pt.peek(2));

        assert_eq!(Some('2'), pt.next());
        assert_eq!(&Some('3'), pt.peek(0));
        assert_eq!(&None, pt.peek(1));
        assert_eq!(&None, pt.peek(2));

        assert_eq!(Some('3'), pt.next());
        assert_eq!(&None, pt.peek(0));
        assert_eq!(&None, pt.peek(1));
        assert_eq!(&None, pt.peek(2));

        assert_eq!(None, pt.next());
        assert_eq!(&None, pt.peek(0));
        assert_eq!(&None, pt.peek(1));
        assert_eq!(&None, pt.peek(2));
    }
}
