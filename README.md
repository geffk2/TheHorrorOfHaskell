# TheHorrorOfHaskell

<details>
<summary>Click here</summary>
Boo
</details>


### Project introduction


In this project we want to create a raytraced horror game like slenderman. Player will need to find some items on the map, while being chased by scary thing. 
[Images to understand idea that we want to implement](#GameImages).

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


### GameImages 
We inpired by the game slenderman.

![SlendermanImage1](https://upload.wikimedia.org/wikipedia/ru/3/39/Slender-Man_Game_Play.jpg)
![SlendermanImage2](https://images.sftcdn.net/images/t_app-cover-l,f_auto/p/485eadd4-9b26-11e6-8371-00163ec9f5fa/1274410765/slender-mac-screenshot.jpg)
