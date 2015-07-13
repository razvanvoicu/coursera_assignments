# implementation of card game - Memory
# http://www.codeskulptor.org/#user40_fHLYp0PHVdo8gSX.py

import simplegui
import random

cards = []
sideUp = []
prev = [-1,-1]
turns = 0
label = 0
sideUpCount = 0

def shuffle():
    cards1 = [0,1,2,3,4,5,6,7]
    cards2 = [0,1,2,3,4,5,6,7]
    for i in range(len(cards1),0,-1):
        cards.append(cards1.pop(random.randrange(0,i)))
        cards.append(cards2.pop(random.randrange(0,i)))
        
# helper function to initialize globals
def new_game():
    global cards, sideUp, turns, prev, sideUpCount;
    cards = []
    shuffle()
    sideUp = [False] * len(cards)
    turns = 0
    prev = [-1,-1]
    sideUpCount = 0

     
# define event handlers
def mouseclick(pos):
    global prev, turns, sideUpCount
    x = pos[0]//50
    if sideUp[x]:
        return
    if sideUpCount == 1:
        turns += 1
    if sideUpCount == 2:
        if cards[prev[0]] != cards[prev[1]]:
            sideUp[prev[0]] = False
            sideUp[prev[1]] = False
        prev = [-1,-1]
        sideUpCount = 0
    sideUp[x] = True
    prev[sideUpCount] = x
    sideUpCount += 1
                        
# cards are logically 50x100 pixels in size    
def draw(canvas):
    if not(type(label) is int):
        label.set_text("Turns = " + str(turns))
    for i in range(0,len(cards)):
        if not sideUp[i]:
            vertices = [(50*i,0),(50*i,99),(50*i+49,99),(50*i+49,0)]
            canvas.draw_polygon(vertices,2,"black","green")
        else:
            canvas.draw_text(str(cards[i]),(50*i+12,65),50,"white")


# create frame and add a button and labels
frame = simplegui.create_frame("Memory", 800, 100)
frame.add_button("Reset", new_game)
label = frame.add_label("Turns = 0")

# register event handlers
frame.set_mouseclick_handler(mouseclick)
frame.set_draw_handler(draw)

# get things rolling
new_game()
frame.start()
