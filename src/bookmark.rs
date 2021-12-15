/*
This is a structure designed to make it easy to delimit the boundaries
of code blocks which then makes it possible to define jump locations
*/

pub struct Bookmark {
    tag: Vec<String>
}