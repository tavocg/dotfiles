#!/usr/bin/env python
help="Usage: sched.py [OPTION] [FILE]\nOptions: new, print, edit."
import sys  # arguments (if given file, just output it with formatting)
import os   # clear console for easier readability

# schedule layout & formatting
weekdays = [
    "sun",
    "mon",
    "tue",
    "wed",
    "thu",
    "fri",
    "sat",
    ]
hours = [
    "05:00",
    "06:00",
    "07:00",
    "08:00",
    "09:00",
    "10:00",
    "11:00",
    "12:00",
    "13:00",
    "14:00",
    "15:00",
    "16:00",
    "17:00",
    "18:00",
    "19:00",
    "20:00",
    ]
colors = [
    '\033[0m',  # 0 - normal
    '\033[90m', # 1 - grey
    '\033[91m', # 2 - red
    '\033[92m', # 3 - green
    '\033[93m', # 4 - yellow
    '\033[94m', # 5 - purple
    '\033[95m', # 6 - magenta
    '\033[96m', # 7 - cyan
    ]
styles = [
    '\033[0m',  # 0 - normal
    '\033[1m',  # 1 - bold
    '\033[4m',  # 2 - underline
    ]
header_align = "center" # center, left, right
entry_align  = "left"   # center, left, right
cell_width   = 13
header_color = 1
header_style = 0

# generate from previous values (utf8 encoded)
def gen_layout():
    # create matrix to fit layout
    placeholder = formatting("~", 1, 0, "center")
    w, h = len(weekdays)+1, len(hours)+1
    schedule = [[placeholder.encode("utf8") for x in range(w)] for y in range(h)]
    # set days & hours in var schedule
    for i in range(1, len(weekdays)+1):
        day=formatting(weekdays[i-1], header_color, header_style, header_align)
        schedule[0][i]=day.encode("utf8")
    for i in range(len(hours)+1):
        hour=colors[1]+"|"+formatting(hours[i-1], header_color, header_style, header_align)
        schedule[i][0]=hour.encode("utf8")
    index=colors[1]+cell_width*" "+" |"+colors[0]
    schedule[0][0] = index.encode("utf8")
    return(schedule)

# formatting
def formatting(entry, color, style, align):
    if align == "left":
        spaces = cell_width-len(entry)-1
        entry = styles[style]+colors[color]+" "+entry+spaces*" "
    if align == "center":
        entry = entry.center(cell_width)
        entry = styles[style]+colors[color]+entry
    if align == "right":
        spaces = cell_width-len(entry)-1
        entry = styles[style]+colors[color]+spaces*" "+entry+" "
    return(entry+styles[0]+colors[1]+"|"+colors[0])

# print schedule (decode)
def print_sched(schedule):
    os.system('cls' if os.name == 'nt' else 'clear')
    decoded_sched = []
    for i in range(len(schedule)):
        decoded_line = []
        for j in range(len(schedule[i])):
             decoded_line.append(schedule[i][j].decode("utf8"))
        decoded_sched.append(decoded_line)
    for row in decoded_sched:
        print(''.join(map(str,row)))

# read saved .csv schedule (utf8 encoded)
def read_sched(file):
    file = open(file, "rb")
    schedule = []
    for line in file:
        i = []
        entries = (line.replace(("\n").encode("utf8"), ("").encode("utf8"))).split((",").encode("utf8"))
        for j in entries:
            i.append(bytes(j))
        schedule.append(i)
    file.close()
    return(schedule)

# save function (utf8 encoded)
def save(schedule, file):
    with open(file, "wb") as f:
        for row in schedule:
            for entry in row:
                f.write(entry + (",").encode("utf"))
            f.write(("\n").encode("utf8"))

# add/edit entry in schedule
def edit(schedule):
    if len(weekdays)+1 != len(schedule[0]) or len(hours)+1 != len(schedule):
        print("Imported and configured layouts don't match! Can't edit, only print.")
        return 0
    day = menu(weekdays, schedule, 0)
    hour = menu(hours, schedule, 0)
    print_sched(schedule)
    print(styles[1]+"\nType what the activity should be:"+styles[0])
    entry = input("-> ")
    if len(entry) > cell_width-2:
        print("Entry is too long for cell_width!")
        return 0
    color = menu(colors, schedule, 1)
    style = menu(styles, schedule, 1)
    entry = formatting(entry, color, style, entry_align)
    schedule[hour+1][day+1] = entry.encode("utf8")

# menu function for opt-lists
def menu(array, schedule, nopt):
    while True:
        print_sched(schedule)
        print(styles[1]+"\nChoose an option:"+styles[0])
        if nopt == 1:
            for i in range(len(array)):
                print(array[i]+str(i)+array[0])

        elif nopt == 0:
            for i in range(len(array)):
                print(styles[1]+str(i)+styles[0]+" - "+array[i])
        try:
            opt = int(input("-> "))
            if 0 <= opt <= len(array):
                return(opt)
            else:
                print("\nOut of range!")
                input("Try again...")
        except:
            print("\nInvalid input! Should be integer.")
            input("Try again...")

# read arguments
if (len(sys.argv)) < 2:
    print(help)
    sys.exit(0)

# main execution
mode = sys.argv[1]
if mode == "new" or mode == "edit":
    if mode == "new":
        schedule = gen_layout()
        print_sched(schedule)
    elif mode == "edit":
        try:
            schedule = read_sched(sys.argv[2])
        except:
            print("Invalid input file!")
    opts = ["print", "edit", "save", "exit"]
    while True:
        print_sched(schedule)
        opt = menu(opts, schedule, 0)
        if opt == 0:        # print
            print_sched(schedule)
        elif opt == 1:      # edit
            print_sched(schedule)
            edit(schedule)
        elif opt == 2:      # save
            print("Type the name of the schedule file (recommended .txt extension):")
            file = input("-> ")
            save(schedule, file)
        elif opt == 3:      # exit
            break
elif mode == "print":
    try:
        schedule = read_sched(sys.argv[2])
    except:
        print("Invalid input file!")
    print_sched(schedule)
