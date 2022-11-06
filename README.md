# amnesty

`amnesty` is my _second_ attempt to write a NES emulator in Haskell.

My first attempt was [honesty](https://github.com/Nick-Chapman/honesty). It was able to run some easy games like Donkey Kong, but at only about 1/2 the required speed. And it never worked on scrolling classics like SMB. It was my first sizeable haskell project; I think I can do better with three years more experience of both Haskell and emulation!

This time I want to be both faster *and* more accurate. More accuracy implies less speed, so... the plan for speed is to apply the ideas of static recompilation which I explored in my [8080/space-invaders](https://github.com/Nick-Chapman/space-invaders) and [z-machine](https://github.com/Nick-Chapman/zagain) projects. This will be a ground up rewrite, scavenging whatever I can from my earlier attempt.
