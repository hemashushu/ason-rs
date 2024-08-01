// Copyright (c) 2023 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions,
// more details in file LICENSE, LICENSE.additional and CONTRIBUTING.

pub struct ForwardIter<'a, T>
where
    T: PartialEq,
{
    upstream: &'a mut dyn Iterator<Item = T>,
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

impl<'a, T> ForwardIter<'a, T>
where
    T: PartialEq,
{
    pub fn new(upstream: &'a mut dyn Iterator<Item = T>, lookahead_length: usize) -> Self {
        let mut buffer = RoundQueue::new(lookahead_length);

        // pre-fill
        for _ in 0..lookahead_length {
            let value = upstream.next();
            buffer.enqueue(value);
        }

        Self {
            upstream,
            buffer,
            lookahead_length,
        }
    }

    pub fn peek(&self, offset: usize) -> Option<&T> {
        assert!(offset < self.lookahead_length);
        self.buffer.peek(offset)
    }

    // pub fn equals(&self, offset: usize, expect: &T) -> bool {
    //     assert!(offset < self.lookahead_length);
    //     let value = self.peek(offset);
    //     matches!(value, Some(actual) if actual == expect)
    // }
}

impl<'a, T> Iterator for ForwardIter<'a, T>
where
    T: PartialEq,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        // take the data from buffer first, and then insert the new data,
        // because the buffer is pre-filled when it is created.
        let value = self.buffer.dequeue();

        let next_value = self.upstream.next();
        self.buffer.enqueue(next_value);

        value
    }
}

#[cfg(test)]
mod tests {
    use crate::forwarditer::ForwardIter;

    #[test]
    fn test_forward_iter() {
        let s = "0123";
        let mut chars = s.chars();
        let mut forward_iter = ForwardIter::new(&mut chars, 3);

        // init
        assert_eq!(Some(&'0'), forward_iter.peek(0));
        assert_eq!(Some(&'1'), forward_iter.peek(1));
        assert_eq!(Some(&'2'), forward_iter.peek(2));
        // assert!(forward_iter.equals(0, &'0'));
        // assert!(forward_iter.equals(1, &'1'));
        // assert!(forward_iter.equals(2, &'2'));

        // consume '0'
        assert_eq!(Some('0'), forward_iter.next());
        assert_eq!(Some(&'1'), forward_iter.peek(0));
        assert_eq!(Some(&'2'), forward_iter.peek(1));
        assert_eq!(Some(&'3'), forward_iter.peek(2));
        // assert!(forward_iter.equals(0, &'1'));
        // assert!(forward_iter.equals(1, &'2'));
        // assert!(forward_iter.equals(2, &'3'));

        // consume '1'
        assert_eq!(Some('1'), forward_iter.next());
        assert_eq!(Some(&'2'), forward_iter.peek(0));
        assert_eq!(Some(&'3'), forward_iter.peek(1));
        assert_eq!(None, forward_iter.peek(2));
        // assert!(forward_iter.equals(0, &'2'));
        // assert!(forward_iter.equals(1, &'3'));

        // consume '2'
        assert_eq!(Some('2'), forward_iter.next());
        assert_eq!(Some(&'3'), forward_iter.peek(0));
        assert_eq!(None, forward_iter.peek(1));
        assert_eq!(None, forward_iter.peek(2));
        // assert!(forward_iter.equals(0, &'3'));

        // consume '3'
        assert_eq!(Some('3'), forward_iter.next());
        assert_eq!(None, forward_iter.peek(0));
        assert_eq!(None, forward_iter.peek(1));
        assert_eq!(None, forward_iter.peek(2));

        // empty
        assert_eq!(None, forward_iter.next());
        assert_eq!(None, forward_iter.peek(0));
        assert_eq!(None, forward_iter.peek(1));
        assert_eq!(None, forward_iter.peek(2));
    }

    #[test]
    fn test_nested_forward_iter() {
        let s = "0123";
        let mut chars = s.chars();
        let mut iter1 = ForwardIter::new(&mut chars, 3);
        let mut iter2 = ForwardIter::new(&mut iter1, 3);

        // init
        assert_eq!(Some(&'0'), iter2.peek(0));
        assert_eq!(Some(&'1'), iter2.peek(1));
        assert_eq!(Some(&'2'), iter2.peek(2));
        // assert!(iter2.equals(0, &'0'));

        // consume '0'
        assert_eq!(Some('0'), iter2.next());
        assert_eq!(Some(&'1'), iter2.peek(0));
        // assert!(iter2.equals(0, &'1'));

        // consume '1'
        assert_eq!(Some('1'), iter2.next());
        assert_eq!(Some(&'2'), iter2.peek(0));
        // assert!(iter2.equals(0, &'2'));

        // consume '2'
        assert_eq!(Some('2'), iter2.next());
        assert_eq!(Some(&'3'), iter2.peek(0));
        // assert!(iter2.equals(0, &'3'));

        // consume '3'
        assert_eq!(Some('3'), iter2.next());
        assert_eq!(None, iter2.peek(0));

        // empty
        assert_eq!(None, iter2.next());
        assert_eq!(None, iter2.peek(0));
    }
}
