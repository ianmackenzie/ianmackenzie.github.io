module Test exposing (md)


md : String
md =
    """# Title with `inline code`

Some `inline code` followed by some _italic_ text.

## A section with `some inline-code`

Containing some `more inline code` as well as [a GitHub link](https://github.com/ianmackenzie/elm-geometry)
and some more text after the link to check wrapping behaviour.

Also some bullets:

* First bullet
* Second bullet
  * With an indented bullet that has a custom widget
    
    <counter/>

  * And another indented bullet with a code block:
    
        some code
        some more code
            some indented code

  * A bullet after the code block!

* A third bullet with [`a fun link`](https://elm-lang.org) and [_an italic link_](https://arstechnica.com)
* Plus a fourth bullet just for fun with some _italic text_ and `long inline code` probably
  splitting over multiple lines because I made this bullet **really** long

## Another _section_
    
With some **bold** text, and a custom element:

<counter/>

A code block:

    code
    more code
        indented code
    unindented code

### A subsection with `some inline-code`

With another custom element:

### `A weird subsection`

Just inline code in the title!

<counter/>
"""
