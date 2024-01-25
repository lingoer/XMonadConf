# Yet another minimal XMonad & Xmobar conf
Why this yet another XMonad conf repo? Well this goes into the question, if you use XMonad with Xmobar together, usually, you saw that Xmobar eats a lot of memory compared to other lightweight top bars (eg. lemonbar i3bar)
The reason is Haskell runtime especially for Xmobar's, which, is heavy.
This repo just demostrate how to try reduce the memory cost of this setup (XMonad & Xmobar).

there are multiple ways to do so:
1. do not use Xmobar ------ I mean, do we really needs such a topbar? In my daily config most of the time, the top bar only showes the XMonad workspaces, window titles, time, and battery infomation. It's not that much infomation and does not fancy enouph to require a powerfull topbar.
2. solution 2 is what this repo showed: if you really needs Xmobar, you might consider build it together with XMonad, since they share one Haskell runtime, the memory might reduce a little bit.
3. solution 3 is what I'm going to demo: keep in mind that the most part of the memory Xmobar eats, is used to allocate runtime "nurseris". When you are using threaded models(which Xmobar do while XMonad don't), GHC allocats one "nursery" for each cores on your machine (by the -with-rtsops=-N flag), that is very powerfull for large applications that needs a lot of GC, but might be a little bit too much for a tiny little topbar. We could tweak this setting to reduce that cost.
4. I'll also show a little bit more solution by my config: since we are dealing with these haskell code as config file, why not draw a topbar directlly in that config with Xlib? this will come later in this repo.


