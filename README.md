# Streamline

Experiments on streaming tools with Haskell.

> Developing on Twitch. Youtube playlist of past streams: https://www.youtube.com/playlist?list=PLyz2muULdPoMkV7QI7tEzNS_Gxe0a4gFE

```bash
$ nix develop
[nix-develop]$ cabal run streamline -- ~/Videos/sample.mkv
```

Launch repl with flakes:
```
nix run .#repl
```

## Thoughts

Aiming to follow in the footsteps of [gi-gtk-declarative by Oscar Wickstrom](https://wickstrom.tech/programming/2018/09/04/declarative-gtk-programming-with-haskell.html):
 1/ finding a graph data structure we like
   a/ [graphs](https://hackage.haskell.org/package/graphs)
   b/ [algebraic-graphs](https://hackage.haskell.org/package/algebraic-graphs)
   c/ [free arrows](http://blog.sigfpe.com/2017/01/building-free-arrows-from-components.html) from Dan Piponi's blog post
   d/ [extensible free arrows](https://www.youtube.com/watch?v=msQiLyExM3w)
   d/ [higher order free arrows](https://stackoverflow.com/questions/12001350/useful-operations-on-free-arrows) from Sjoerd  Visscher
   e/ [parametric higher order abstract syntax - PHOAS structured graphs](https://www.youtube.com/watch?v=tQGh5oemhkw)
   f/ PHOAS free arrows ?
 2/ using this graph data structure to represent the diffable state for a declarative GStreamer pipeline API

