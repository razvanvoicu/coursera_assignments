# http://www.codeskulptor.org/#user40_3Odm9NtYkTz7kUW.py

# Implementation of classic arcade game Pong

import simplegui
import random

# initialize globals - pos and vel encode vertical info for paddles
WIDTH = 600
HEIGHT = 400       
BALL_RADIUS = 20
PAD_WIDTH = 8
PAD_HEIGHT = 80
HALF_PAD_WIDTH = PAD_WIDTH / 2
HALF_PAD_HEIGHT = PAD_HEIGHT / 2
LEFT = False
RIGHT = True

INK = "White"

X = 0
Y = 1

SCORE_FONT_SIZE = 40

PAD_VEL = 10
BALL_ACCEL = 1.5
HORIZ_SPEED = 2.0

score1 = 0
score2 = 0

def rand_vert_speed():
    return -(0.1+2*random.random())

ball_pos = [WIDTH/2,HEIGHT/2]
ball_vel = [HORIZ_SPEED,rand_vert_speed()]
paddle1_pos = HEIGHT/2
paddle1_vel = 0
paddle2_pos = HEIGHT/2
paddle2_vel = 0

# initialize ball_pos and ball_vel for new bal in middle of table
# if direction is RIGHT, the ball's velocity is upper right, else upper left
def spawn_ball(direction):
    global ball_pos, ball_vel # these are vectors stored as lists
    ball_pos = [WIDTH/2,HEIGHT/2]
    if direction:
        ball_vel = [HORIZ_SPEED,rand_vert_speed()]
    else :
        ball_vel = [-HORIZ_SPEED,rand_vert_speed()]


# define event handlers
def new_game():
    global paddle1_pos, paddle2_pos, paddle1_vel, paddle2_vel  # these are numbers
    global score1, score2  # these are ints
    score1 = 0
    score2 = 0
    paddle1_pos = HEIGHT/2
    paddle1_vel = 0
    paddle2_pos = HEIGHT/2
    paddle2_vel = 0
    if random.random() < 0.5 :
        spawn_ball(LEFT)
    else :
        spawn_ball(RIGHT)

def draw(canvas):
    global score1, score2, paddle1_pos, paddle2_pos, ball_pos, ball_vel
        
    # draw mid line and gutters
    canvas.draw_line([WIDTH / 2, 0],[WIDTH / 2, HEIGHT], 1, INK)
    canvas.draw_line([PAD_WIDTH, 0],[PAD_WIDTH, HEIGHT], 1, INK)
    canvas.draw_line([WIDTH - PAD_WIDTH, 0],[WIDTH - PAD_WIDTH, HEIGHT], 1, INK)
        
    # update ball
    ball_pos = [ball_pos[X]+ball_vel[X],ball_pos[Y]+ball_vel[Y]]
            
    # draw ball
    canvas.draw_circle(ball_pos,BALL_RADIUS,1,INK,INK)
    
    # update paddle's vertical position, keep paddle on the screen
    paddle1_pos += paddle1_vel
    paddle2_pos += paddle2_vel
    pad_upper_limit = PAD_HEIGHT/2 - 1
    pad_lower_limit = HEIGHT - PAD_HEIGHT/2 - 1
    if paddle1_pos < pad_upper_limit:
        paddle1_pos = pad_upper_limit
    if paddle1_pos > pad_lower_limit:
        paddle1_pos = pad_lower_limit
    if paddle2_pos < pad_upper_limit:
        paddle2_pos = pad_upper_limit
    if paddle2_pos > pad_lower_limit:
        paddle2_pos = pad_lower_limit

    # draw paddles
    canvas.draw_line([HALF_PAD_WIDTH,paddle1_pos-PAD_HEIGHT/2],
                     [HALF_PAD_WIDTH,paddle1_pos+PAD_HEIGHT/2],
                     PAD_WIDTH,INK)
    canvas.draw_line([WIDTH-HALF_PAD_WIDTH,paddle2_pos-PAD_HEIGHT/2],
                     [WIDTH-HALF_PAD_WIDTH,paddle2_pos+PAD_HEIGHT/2],
                     PAD_WIDTH,INK)
    
    # determine whether paddle and ball collide
    if ball_pos[Y] <= BALL_RADIUS-1 or ball_pos[Y] >= HEIGHT-BALL_RADIUS-1:
        ball_vel = [ball_vel[X],-ball_vel[Y]]
    
    ball_gutter1 = ball_pos[X] <= BALL_RADIUS+PAD_WIDTH-1 
    pad1_top = ball_pos[Y] >= paddle1_pos - PAD_HEIGHT/2 - 1
    pad1_bot = ball_pos[Y] <= paddle1_pos + PAD_HEIGHT/2 - 1
    reflect_pad1 = ball_gutter1 and pad1_top and pad1_bot
    ball_gutter2 = ball_pos[X] >= WIDTH-BALL_RADIUS-PAD_WIDTH-1
    pad2_top = ball_pos[Y] >= paddle2_pos - PAD_HEIGHT/2 - 1
    pad2_bot = ball_pos[Y] <= paddle2_pos + PAD_HEIGHT/2 - 1
    reflect_pad2 = ball_gutter2 and pad2_top and pad2_bot
    
    if reflect_pad1 or reflect_pad2:
        ball_vel = [-BALL_ACCEL*ball_vel[X],BALL_ACCEL*ball_vel[Y]]
    
    if ball_gutter1 and not (pad1_top and pad1_bot) :
        score2 += 1
        spawn_ball(RIGHT)
    
    if ball_gutter2 and not (pad2_top and pad2_bot) :
        score1 += 1
        spawn_ball(LEFT)
    
    # draw scores
    canvas.draw_text(str(score1),[WIDTH/4,HEIGHT/4],SCORE_FONT_SIZE,INK)
    canvas.draw_text(str(score2),[3*WIDTH/4,HEIGHT/4],SCORE_FONT_SIZE,INK)
        
def keydown(key):
    global paddle1_vel, paddle2_vel
    if key == simplegui.KEY_MAP["w"]:
        paddle1_vel = -PAD_VEL
    if key == simplegui.KEY_MAP["s"]:
        paddle1_vel = PAD_VEL
    if key == simplegui.KEY_MAP["up"]:
        paddle2_vel = -PAD_VEL
    if key == simplegui.KEY_MAP["down"]:
        paddle2_vel = PAD_VEL
   
def keyup(key):
    global paddle1_vel, paddle2_vel
    if key == simplegui.KEY_MAP["s"] or key == simplegui.KEY_MAP["w"]:
        paddle1_vel = 0
    if key == simplegui.KEY_MAP["up"] or key == simplegui.KEY_MAP["down"]:
        paddle2_vel = 0


# create frame
frame = simplegui.create_frame("Pong", WIDTH, HEIGHT)
frame.set_draw_handler(draw)
frame.set_keydown_handler(keydown)
frame.set_keyup_handler(keyup)
frame.add_button("Restart", new_game)


# start frame
new_game()
frame.start()
