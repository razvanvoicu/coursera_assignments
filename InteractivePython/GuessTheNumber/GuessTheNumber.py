# http://www.codeskulptor.org/#user40_91Qe3HDYVaKG1DU.py

import simplegui
import random
# template for "Guess the number" mini-project
# input will come from buttons and an input field
# all output for the game will be printed in the console

low = 0
high = 0
steps_left = 0
secret_guess = 0
range = None

def banner():
    print "New game. Range is from", low, "to", high
    remaining_guesses()
    print ""

def remaining_guesses():
    print "Number of remaining guesses is", steps_left

def print_correct():
    print "Correct!"
    print ""

def print_higher():
    print "Higher!"
    print ""
    
def print_lower():
    print "Lower!"
    print ""

def print_out_of_guesses(secret_guess):
    print "You ran out of guesses. The number was", secret_guess
    print ""

def new_game():
    """Start and restart the game"""
    range()


# define event handlers for control panel
def range100():
    """Initializes a 0-100 game"""
    global low, high, steps_left, range, secret_guess
    low = 0
    high = 100
    steps_left = 7
    range = range100
    banner()
    secret_guess = random.randint(low,high)
 
def range1000():
    """Initializes a 0-1000 game"""
    global low, high, steps_left, range, secret_guess
    low = 0
    high = 1000
    steps_left = 10
    range = range1000
    banner()
    secret_guess = random.randint(low,high)
    
def input_guess(guess):
    """Event handler for text input"""
    global steps_left
    steps_left -= 1
    print "Guess was",guess
    print "Number of remaining guesses is", steps_left
    numeric_guess = int(guess)
    if numeric_guess == secret_guess:
        print_correct()
        new_game()
    elif steps_left == 0:
        print_out_of_guesses(secret_guess)
        new_game()
    elif numeric_guess < secret_guess:
        print_higher();
    else:
        print_lower();
    
    
# create frame
f = simplegui.create_frame("Guess the number", 200, 200)

# register event handlers for control elements and start frame
f.add_button("Range is [0,100)", range100, 200)
f.add_button("Range is [0,1000)", range1000, 200)
f.add_input("Enter a guess", input_guess, 200)

# call new_game 
range = range100
new_game()

f.start()
