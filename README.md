# no_xml - streaming XML parser with no_std support

Why have I done this? No idea. I think I was writing a custom programming language at one point that integrated a JSX-like syntax that would work within XML. And for some reason I thought "huh, I wonder if I can do a heapless XML parser." And I guess, here we are.

Suddenly remembered the code existed today and decided to publish it.

Does it work? Seems to. It's not complete but it does parse a large subset of standard XML.

Is it secure? Well it might crash but it can't do anything clever like connect to the Internet so you should be fine.

