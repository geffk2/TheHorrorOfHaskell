
# TheHorrorOfHaskell

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)


<details>
<summary>Click here</summary>
Boo
</details>


# Links
[GitHub Projects (to see process)](https://github.com/users/geffk2/projects/1/views/1)

* [Introduction](#introduction)
  * [Project description](#project-description)
  * [Raytracing](#raytracing)
  * [Characters and map designing](#characher-and-map-designing)
  * [NPC](#npc-ai)
  * [Game images](#game-images)
* [Installation](#installation)
  * [Requirements](#requirements)
  * [How to run](#run-game)
* [Gameplay](#gameplay)
  * [Game story](#game-story)
  * [Controls](#controls)
* [Development](#development)
  * [Raycasting implementation](#raycasting-implementation)
  * [NPC brain](#npc-algorithm-implementation)

# Introduction
### Project description


In this project we want to create a raytraced horror game like slenderman. Player will need to find some items on the map, while being chased by scary thing. 
[Images to understand idea that we want to implement](#game-images).

This project seems interesting to us because it contains a lot of stept:
* Raytracing
* Map designing
* NPC with some pseudo AI (as horror character)

We have exactly 3 team members, so we descided to distribute our roles like this:
* Ekaterina Maximova - responsible for raytracing and testing (because other members are too scared to test)
* Polina Zelenskaya - responsible for map and charachers designing
* Danila Kuzmin - responsible for NPC AI


### Raytracing

We have seen that CodeWorld already have [example](https://code.world/haskell#PSAP49qms7Ahg4z1gpLm2Jw) of game raytracing.
We want to try to implement more effective and complex type to render not only blocks, but more complex elements.

![CodeWorldRaytracing](https://i.imgur.com/bWfZ6s0.png)


### Characher and map designing

It is important to understand how to render complex objects and how to make game lightweighted, but with good graphics.
Also due to the fact that game should be horror, it is essential to try to create horror atmosphere. Like characters below are not scary at all.

![Non horror character example](https://www.tatar-inform.ru/resize/shd/images/uploads/news/2020/8/28/737f213bcc483f2947ad1005163ad0664ac3fd7c8a9723a0d8cbeb313196.jpg)


### NPC AI

One of the most important part in horror games is NPC logic. NPC should pursue, but not always. NPC should scare, but not always. And so on. 
So NPC logic is complex thing that make most of the game. Good example of NPC logic is in one video that I am struggling to find, however key points are that:
* There is no NPC (only scary sounds) till player find something
* NPC walking randomly till player achieve something
* Then NPC is chasing player

That's one example and how it will be implemented will be discussed with team


### Game images 
We inpired by the game slenderman.

![SlendermanImage1](https://upload.wikimedia.org/wikipedia/ru/3/39/Slender-Man_Game_Play.jpg)
![SlendermanImage2](https://images.sftcdn.net/images/t_app-cover-l,f_auto/p/485eadd4-9b26-11e6-8371-00163ec9f5fa/1274410765/slender-mac-screenshot.jpg)



# Installation

### Requirements
This game is written on Haskell, so you should download [gch](https://www.haskell.org/downloads/) and [cabal](https://www.haskell.org/cabal/).
```
ghc-8.10.7
cabal-3.6
```


### Run game

Type in shell two command:
```shell
$ cabal build
$ cabal run
```
If window `boo` appeared, you succesfully finished your configuration!


# Gameplay

### Game story

Your goal is to find all buttons to escape from old creppy house.
However there you are not alone in the house, something strange is also with you. But don't mind it, it e̷v̴e̴r̶y̸t̴h̵i̸n̸g̴ ̴w̷i̴l̶l̶ ̴b̴e̴ ̵a̶l̵r̴i̷g̶h̸t̸


### Controls
This game require only keyboard, so here is the list of all buttons and their functionallity:

* `w - move forwars`
* `a - turn left` 
* `s - move backeards`
* `d - turn right`

Important to mention that all buttons are **key sensitive**!



# Development 

### Raycasting implementation

For walls we used `Graphics.Gloss.Algorithms.RayCast.castSegIntoCellularQuadTree` which is already build-in tool to detect where 'ray' intersected with object (Coords, Tile, Side). For more info check [documentation](https://hackage.haskell.org/package/gloss-algorithms-1.13.0.3/docs/Graphics-Gloss-Algorithms-RayCast.html).


For textures we implemeted `hitToTexture` function which by hitPoint and Side draw texture. Implementation could be found [here](https://github.com/geffk2/TheHorrorOfHaskell/blob/e822705e0cbf956dd23b31789529a4e9a9b99cdb/app/Main.hs#L151).

### NPC algorithm implementation

