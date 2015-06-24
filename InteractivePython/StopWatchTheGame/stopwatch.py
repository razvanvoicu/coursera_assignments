# http://www.codeskulptor.org/#user40_1JPIRnDs2mW4TXo.py

import simplegui

# define global variables
counter = 0   # current time in 1/10s of second
stops   = 0   # number of times 'Stop' was pressed since last reset
wins    = 0   # number of times 'Stop' was pressed on the second since last reset

started = False # toggles the running/paused state of the counter

interval = 100  # time interval between ticks

# define helper function format that converts time
# in tenths of seconds into formatted string A:BC.D
def format(t):
    msec = t % 10
    secs = (t / 10) % 10
    tens = (t % 600) / 100
    mins = t / 600
    return str(mins) + ":" + str(tens) + str(secs) + "." + str(msec)
    
# define event handlers for buttons; "Start", "Stop", "Reset"
def start():
    global started
    started = True

def stop():
    global started, stops, wins
    if started :
        started = False
        stops += 1
        if (counter % 10 == 0) :
            wins += 1

def reset():
    global started, counter, stops, wins
    started = False
    counter = 0
    stops = 0
    wins = 0

# define event handler for timer with 0.1 sec interval
def tick():
    global counter
    if started :
        counter += 1

# define draw handler
def draw(canvas):
    canvas.draw_text(format(counter),[96, 112], 48, "White")
    canvas.draw_text(str(wins) + "/" + str(stops), [260,24], 24, "Green")
    
# create frame
frame = simplegui.create_frame("Stop Watch: The Game", 300, 200)
frame.add_button("Start", start, 100)
frame.add_button("Stop", stop, 100)
frame.add_button("Reset", reset, 100)
timer = simplegui.create_timer(interval, tick)

# register event handlers
frame.set_draw_handler(draw)

# start frame
frame.start()
timer.start()
