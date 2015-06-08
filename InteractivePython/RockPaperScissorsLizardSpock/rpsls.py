# Rock-paper-scissors-lizard-Spock solution
# http://www.codeskulptor.org/#user40_VPfBSamYkKf1SOE.py

import random


# helper functions

# using a dictionary is faster than if-elif-else
name_to_number_dict = {
    "rock"     : 0,
    "Spock"    : 1,
    "paper"    : 2,
    "lizard"   : 3,
    "scissors" : 4
    }

# perform conversion by looking up the argument into the dictionary
def name_to_number(name):
    """Converts rock->0, Spock->1, paper->2, lizard->3, scissors->4"""
    return name_to_number_dict[name]

# using a list is faster here
number_to_name_dict = [
    "rock",
    "Spock",
    "paper",
    "lizard",
    "scissors"
    ]

# perform conversion by looking up the argument in the list
def number_to_name(number):
    """Converts 0->rock, 1->Spock, 2->paper, 3->lizard, 4->scissors"""
    return number_to_name_dict[number]
    

# main function
def rpsls(player_choice):
    """Plays one round of Rock-paper-scissors-lizard-Spock"""
    # print a blank line to separate consecutive games
    print ""

    # print out the message for the player's choice
    print "Player chooses ",player_choice
    
    # convert the player's choice to player_number using the function name_to_number()
    player_number = name_to_number(player_choice)
    
    # compute random guess for comp_number using random.randrange()
    computer_number = random.randrange(0,5)
    
    # convert comp_number to comp_choice using the function number_to_name()
    computer_choice = number_to_name(computer_number)
    
    # print out the message for computer's choice
    print "Computer chooses ",computer_choice
    
    # compute difference of comp_number and player_number modulo five
    diff = (computer_number - player_number) % 5
    
    # use if/elif/else to determine winner, print winner message
    if diff == 0 : print "Player and computer tie."
    elif diff == 1 or diff == 2 : print "Computer wins."
    elif diff == 3 or diff == 4 : print "Player wins."

# test your code - THESE CALLS MUST BE PRESENT IN YOUR SUBMITTED CODE
rpsls("rock")
rpsls("Spock")
rpsls("paper")
rpsls("lizard")
rpsls("scissors")

# always remember to check your completed program against the grading rubric
