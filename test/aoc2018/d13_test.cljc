(ns aoc2018.d13-test
  (:require [aoc2018.d13 :refer [solve-1 solve-2]]
            [clojure.test :refer [deftest is testing]]))


(def example "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   ")


(def example-2 "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/")


(def input "                    /--------------------------------------------------\\                                        /----------------\\                    \n          /---------+--------------------------------------------------+-------------\\                          |                |/-------------\\     \n          |   /-----+---------------------------------\\                |          /--+--------------------------+-------\\        ||             |     \n          |   |     |                                 |        /-------+----------+--+--------------------------+-------+---\\    ||             |     \n          |   |     |              /------------------+--------+-------+\\         |  |                          |       |   |    ||             |     \n          |   |     |    /---------+------------------+--------+--\\    ||         |  |                    /-----+-------+---+----++-------------+---\\ \n          |   |     |    |         |                  |        |  |    ||         |  |                    |     |       |   |    ||             |   | \n        /-+---+-----+----+---\\     |                  |        |  |    ||         |/-+--------------------+-----+-------+---+----++-------------+\\  | \n        | |   |     |    |   |     |   /--------------+--------+--+----++---------++-+--------------------+-----+------\\|   |    ||             ||  | \n        | |/--+-----+----+---+-----+---+--------------+--------+--+----++\\        || |                    |     |      ||   |    ||             ||  | \n        | ||  |     |    |   |     |   |              |        |  |   /+++--------++-+--------------------+\\    |      ||   |    ||             ||  | \n        | ||  |   /-+----+---+-----+---+--------------+--\\     |  |   ||||        || |  /-----------------++\\   |      ||   |    ||             ||  | \n        | || /+---+-+--\\ |   |     |   |              |  |     |  |   ||||        || |  |                 |||   |      ||   |    ||             ||  | \n      /-+-++-++---+-+--+-+---+-----+---+--------------+--+-----+--+---++++--------++-+--+----------\\      |||   |      ||   |    ||             ||  | \n      | | || ||   | |  | |   |     |/--+--------------+\\ |     |  |   ||||        || |  |          |      |||   |      ||   |    |\\-------------/|  | \n      | | || ||   | |  | |   |/----++--+--------------++-+-----+--+---++++--------++-+--+----------+---\\  |||   |      ||   |    |               |  | \n      | | || ||  /+-+--+-+---++----++--+--------------++-+-----+--+---++++--------++-+--+----------+---+--+++\\  |      ||   |    |               |  | \n      | | || ||  || |  | |   ||    ||  |       /------++-+-----+--+---++++--------++-+--+----------+---+--++++--+---\\  ||   |    |               |  | \n      | | || ||  || |  | |   ||    ||  |       |      || |     |  |   ||||        || |  |          |   |  ||||  |   |  ||   |    |               |  | \n      | | || ||  || |  | |   ||    || /+-------+------++-+-----+--+---++++---->---++-+--+----------+---+--++++--+---+--++---+-\\  |               |  | \n      | | || ||  || |  | |   ||    || ||       |      || |     |  |   ||||        || |  |          |   |  ||||  |   |  ||   | |  |               |  | \n      | | || ||  || |  | |   ||    || ||       |      || |     |  |   ||||        || |  |          |   |  ||||  |   |  ||   | |  |               |  | \n      | | ||/++--++-+--+-+---++\\   || ||       |      || |     |  |   ||||        || |  |          |   |  ||||  |   |  ||   | |  |               |  | \n     /+-+-+++++\\ || |  | |   |||   || ||       |      || |/----+--+---++++--------++-+--+----------+---+--++++--+---+--++---+-+-\\|               |  | \n /---++-+-++++++-++-+--+-+---+++---++-++----->-+------++-++----+--+---++++--------++\\|  |    /-----+---+--++++--+---+--++---+-+-++---------------+\\ | \n |   || | |||||| || \\--+-+---+++---++-++-------+------++-++----+--+---+/||        |\\++--+----+-----+---+--++++--+---+--++---+-+-++---------------/| | \n |   || | |||||| ||    | |   |||   || ||     /-+------++-++----+--+---+-++--------+-++--+----+-----+---+--++++--+---+--++--\\|/+-++------------\\   | | \n |   || | |||||| ||    | |   |||   || ||     | |  /---++-++----+-\\|  /+-++--------+-++--+----+-----+-\\ |  ||||  |   |  ||  |||| ||            |   | | \n |   || | |||||| ||    | |   |||   || ||     | |  |   || ||   /+-++--++-++--\\     | ||/-+----+-----+-+-+--++++--+---+--++--++++-++--\\         |   | | \n |   |\\-+-++++++-++----+-+---+++---++-++-----+-+--+---++-++---++-++--++-++--+-----+-+++-+----+-----/ | |  ||||  |   |  ||  |||| ||  |         |   | | \n |   |  | |||||| ||    | |   |||   || ||     | |  |   || ||   || ||  || ||  |     | ||| |    |       | |  ||||  |   | /++--++++-++--+--------\\|   | | \n |   |  | |||||| ||    | |   |||   || ||  /--+-+--+---++-++---++-++--++-++--+-----+-+++-+----+--\\    | |  ||||  |   | |||  |||| ||  |        ||   | | \n |   |  | |\\++++-++----+-+---+++---++-++--+--+-+--+---++-++---++-++--++-+/  |     | ||| |  /-+--+----+-+--++++--+---+-+++--++++-++--+----\\   ||   | | \n |   |  | \\-++++-++----+-+---+++---++-++--+--+-+--+---++-++---++-++--++-+---+-----+-+/| |  | |  |/---+-+--++++--+---+-+++--++++-++-\\|    |   ||   | | \n |   |  |   |||| \\+----+-+---+++---++-++--+--+-+--+---++-++---++-++--++-+---+-----+-+-+-+--+-+--++---+-+--+++/  |   | |||  |||| || ||    |   ||   | | \n |   |  |   ||||  |    | |   |||   || ||  |  | |  |   || ||   || ||  || |   |     | | | |  | |  ||   | |  |||   |   | |||  |||| || ||    |   ||   | | \n |   |  |   ||||  |    | |   |||  /++-++--+--+-+--+---++-++---++-++--++-+---+----\\| | | |  | |  ||   | |  |||   |   | |||  |||| || ||    |   ||   | | \n |   |  \\---++++--+----+-+---/||  ||| ||  |  | |  |   || ||  /++-++--++-+\\  |    || | | |  | |  ||   | |  |||   |   | |||/-++++-++-++----+---++---+-+\\\n |   |      ||||  |    | |    ||  ||| ||  |  | |  |   || ||  ||| ||  || ||  |    || | | |  | |  ||   | | /+++---+---+\\|||| |||| || ||    |   ||   | ||\n |   |      ||||  |   /+-+----++--+++-++--+--+-+--+---++-++\\ ||| ||  || ||  | /--++-+-+-+-\\|/+--++---+-+-++++---+---++++++-++++-++-++----+---++--\\| ||\n |   \\------+++/  |   || |    ||  ||| ||/-+--+-+--+---++-+++-+++-++--++-++--+-+--++-+-+-+-++++--++---+-+-++++-\\ |   |||||| |||| || ||    |   ||  || ||\n |          |||   |   || |    ||  ||| ||| |  | |  |   || |||/+++-++--++-++--+-+--++-+-+-+-++++->++---+-+-++++-+-+---++++++-++++-++-++--\\ |   ||  || ||\n |          |||   |   || |    ||  ||| ||| \\--+-+--+---++-+++++++-++--++-++--+-+--++-+-+-+-++++--/|   | | |||| | |   |||||| |||| || ||  | |   ||  || ||\n |          |||  /+---++-+----++--+++-+++----+-+--+---++-+++++++-++--++-++--+-+--++-+-+-+-++++--\\|   | | |||| | |   |||||| |||| || ||  | |   ||  || ||\n |          |||  ||   || |    ||  ||| |||    | |  \\---++-+++++++-/|  || ||  | |  || | | | |\\++--++---+-+-++++-+-+---++++++-++++-++-++--+-/   ||  || ||\n |          |||  ||   || |    ||  ||v |||    | |      || |||||||  |  || ||  | |  || | | |/+-++--++---+-+-++++-+-+---++++++-++++-++-++-\\|     ||  || ||\n |          |||/-++---++-+----++--+++-+++----+-+------++-+++++++--+--++-++--+-+--++-+-+-+++-++--++---+-+-++++-+-+--\\|||||| ||||/++-++-++-\\   ||  || ||\n |          |||| ||   || |   /++--+++-+++----+-+------++-+++++++-\\|  \\+-++--+-+--++-+-+-+++-++--++---/ | |||| | |  ||||||| ||||||| || || |   ||  || ||\n |          |||| ||   || |   |||  ||| \\++----+-+------++-+++++++-++---+-++--+-+--++-+-+-+++-++--++-----+-++++-+-+--+++++++-+++/||| || || |   ||  || ||\n |          |||| ||   || |   |||  |||  ||    | |      || ||||||| ||   | ||  | |  || | | ||| ||  ||     | |||| | |  ||||||| ||| ||| || || |   ||  || ||\n |          |||| || /-++-+---+++--+++--++----+-+------++-+++++++-++---+-++--+-+--++-+-+-+++-++--++-----+-++++-+-+-\\||||||| ||| ||| || || |   ||  || ||\n |          |||| || | || |   |||  |||  ||    |/+------++-+++++++-++---+-++--+\\|  || | | ||| ||/-++-----+-++++-+-+-++++++++-+++-+++\\|| || |   ||  || ||\n |  /-------++++-++-+-++-+---+++--+++--++----+++------++-+++++++-++-\\ | ||  |||  || | | ||| ||| ||     | |||| | | |||||||| ||| |||||| || |   ||  || ||\n |  |       |||| || | || |   |||  |||  ||    |||      || ||||||| || | | ||  |||  || | | ||| ||| ||     | |||| | | |||||||| ||| |||||| || |   ||  || ||\n |  |       |||| || | || |   |||  |||  ||    |||      || ||||||| || | | ||  |||  || | | ||| ||| ||     | |||| | |/++++++++-+++-++++++-++-+-\\ ||  || ||\n |  |       |||| || | || |   |||  |||  ||    |||      || ||||||| || | ^ ||  |||  || | | ||| ||| ||     | |||| | |||||||||| ||| |||||| || | | ||  || ||\n |  |       |||| || | ||/+---+++--+++--++----+++\\     || ||||||| || | | ||  |||  || | | |^| ||| |\\-----+-++++-+-++++++++++-+++-++++/| || | | ||  || ||\n |  |       |||| || | ||||   |||  ||\\--++----++++-----+/ ||||||| || | | ||  |||  || | | ||| ||| |      | |||| | |||||||||| ||| |||| | || | | ||  || ||\n |  |  /----++++-++-+-++++---+++--++---++----++++-----+--+++++++-++-+-+-++--+++\\ || | | ||| ||| |      | |||| | |||||||||\\-+++-++++-+-++-+-+-++--++-+/\n |  |  |    |||| || | ||||   |||  ||   ||    ||||     |  ||||||| || | | ||  |||| || | \\-+++-+++-+------+-++++-+-+++++++++--+++-++++-/ || | | ||  || | \n |  |  |    |||| || | ||||   |||  ||   ||    ||||     |  ||||||| || |/+-++--++++-++-+---+++-+++-+------+-++++-+-+++++++++--+++\\||||   || | | ||  || | \n |  |  |    |||| |\\-+-++++---+++--++---++----++++-----+--/|||||| || ||| ||  |||| || |   ||| ||| |      | |||| | |||||||||  |^||||||   || | | ||  || | \n |  |  |    |||| |  | ||||   |||/-++---++----++++-----+---++++++-++-+++-++--++++-++-+---+++-+++-+----\\ | |||| | |||||||||  ||||||||   || | | ||  || | \n |  |  |    |||| |  | ||||   |||| ||   ||    ||||     |   |||||| || ||| ||  |||| || |   ||| \\++-+----+-+-++++-+-+++++++++--++++++++---++-+-+-++--/| | \n |  |  |  /-++++-+--+-++++---++++-++---++----++++-----+---++++++-++-+++-++-\\|||| || |   |||  || |    | | |||| | |||||||||  ||||||||   || | | ||   | | \n |  |  |  | |||| |  | ||||   |||| ||   ||    ||||     |   |||||| || ||| || ||||| || |   |||  || |    | | |||| | \\++++++++--++++++/|   || | | ||   | | \n |  |  |  | |||| |  | ||||   |||| ||   ||    ||||     |  /++++++-++-+++-++-+++++-++-+---+++--++\\|    | | |||| |  ||||||||  |||||| |   || | | ||   | | \n |  |  |  | |||| |  | ||||   |||| ||   ||    ||||     |  |||||||/++-+++-++-+++++-++-+---+++--++++----+-+\\|||| |  ||||||||  |||||| |   || | | ||   | | \n |  |  | /+-++++-+--+-++++---++++-++---++----++++-----+--++++++++++-+++-++-+++++-++-+-\\ |||  ||||    | |||||| |  ||||||||  |||||| |   || | | ||   | | \n |  |  | || |||| |  | ||||   |||| ||   \\+----++++-----+--++++++++++-+++-++-+++++-++-+-+-+++--++++----+-++++++-+--++++++/|  |||||| |   || | | ||   | | \n |  |  | || |\\++-+--+-+/||   |||| ||    |    ||||     |  |||||||||| ||\\-++-+++++-++-+-+-+++--++++----+-++++/| |  |||||| |  |||||| |   || | | ||   | | \n |  |  | || | || |  | | ||   |||| ||    |    ||||     |  |||||\\++++-++--++-+/||| || | |/+++--++++----+-++++-+-+--++++++-+--++++++-+-\\ || | | ||   | | \n |/-+--+-++-+-++-+--+-+\\||   |||| ||    |    ||||     |  ||||| |||| ||  || | ||| || | |||||  ||||    | |||\\-+-+--++++++-+--++++++-+-+-++-+-+-++---+-/ \n || |  | || | || |  | ||||   |||| ||    |    ||||     |  ||||| |||| ||  || | ||| || | |||||  ||||    | |||/-+-+--++++++-+--++++++-+-+-++-+\\| ||   |   \n || |  | || | || |  | ||||   |||| ||    | /--++++-----+--+++++-++++-++--++-+-+++-++-+-+++++--++++----+-++++-+-+--++++++-+--++++++-+-+-++-+++-++---+--\\\n || |  | || | || |  | \\+++---++++-++----+-+--++++-----+--++/|| |||| ||  || | |\\+-++-+-++++/  ||||    | |||| | |  |||||| |  |||||| | | || ||| ||   |  |\n || |  | || | || |  |  |||   |||| ||    | |  ||||   /-+--++-++-++++-++--++-+-+-+-++-+-++++---++++----+-++++-+-+--++++++-+-\\|||||| | | || ||| ||   |  |\n || |  | || \\-++-+--+--+++---++/| ||    | |  ||||   | |  || || |||| ||  || | | | || | ||||  /++++----+-++++-+-+--++++++-+-+++++++-+-+-++\\||| ||   |  |\n || |  | ||   || |  |  |||   || | |\\----+-+--++++---+-+--++-++-++++-++--/| | | | ||/+-++++--+++++----+-++++-+-+--++++++-+-+++++++\\| | |||||| ||   |  |\n || |  | ||   || |  |  |||   || | |     | |  ||||   | |  || || |||| ||   | | | | |||| ||||  |||||    | |||| | |  |||||| | ||||||||| | |||||| ||   |  |\n || |  | ||   || |  \\--+++---++-+-+-----+-+--++++---+-+--++-++-++++-++---+-+-+-+-++++-++++--+++++----+-++++-+-+--+/|||| | ||||||||| | |||||| ||   |  |\n || |  | ||  /++-+-----+++\\  || | \\-----+-+--++++---+-+--++-++-++++-++---+-+-+-+-/||| ||||  |||||    | |||| | |  | |||| | ||||||||| | |||||| ||   |  |\n || |  | ||  |||/+-----++++--++-+----\\  | |  ||||   | |  || || |||| ||   | | | |  ||| ||||  |||||    | |||| | |  | |||\\-+-+++++++++-+-++++++-/|   |  |\n || |  | ||  ||\\++-----++++--++-+----+--+-+--++++---+-+--++-++-++++-++---+-+-+-+--+++-++++--+++++----+-++++-+-+--+-/||  | ||||||||| | ||||||  |   |  |\n || |  ^ ||  || ||     |||| /++-+----+--+-+\\ ||||   | |  || || |||| ||   | | | |  ||| ||||  ||\\++----+-++++-+-+--+--++--+-++++++++/ | ||||||  |   |  |\n || |  | ||  || ||     |||| ||| |    |  | || ||||   \\-+--++-++-++++-++---+-+-+-+--+++-++++--++-++----+-++++-+-+--+--++--+-/|||||||  | ||||||  |   |  |\n || |  | ||/-++-++-----++++\\||| |    |  | || ||||     |  || || |||| |\\---+-+-+-+--+++-++++--++-++----+-++++-+-+--+--++--+--+++/|||  | ||||||  |   |  |\n || |  | ||| |\\-++-----++++++++-+----+<-+-++-++++-----/  || || |||| |    | | | |  ||| ||||  || ||    | ||\\+-+-+--+--+/  |  ||| |||  | v|||||  |   |  |\n \\+-+--+-+++-+--++-----++++++++-+----+--+-++-++++--------++-++-++++-+----+-+-+-+--++/ ||||  || ||/---+-++-+-+-+--+--+---+--+++-+++--+-++++++\\ |   |  |\n  | |  | ||| |  ||     |||||||| |    |  | || ||||        || || |||| |    | | | |  |\\--++++--++-+++---+-++-+-+-+--+--+---+--+++-++/  | ||||||| |   |  |\n  | | /+-+++-+--++-----++++++++-+----+--+-++-++++----\\   || || \\+++-+----+-+-+-+--+---++++--++-+++---+-++-+-+-+--+--+---+--+/| ||   | ||||||| |   |  |\n  | | || |||/+--++-----++++++++-+----+--+-++\\||||    |   || \\+--+++-+----+-+-+-+--+---++++--++-+++---+-++-+-+-+--+--+---+--+-+-++---+-+/||||| |   |  |\n/-+-+-++-+++++--++-----++++++++-+----+--+-+++++++----+---++-\\|  \\++-+----+-+-+-+--+---++++--++-+++---+-+/ | | |  |  |   |  | | ||   | | ||||| |   |  |\n| | | || |||||  ||     |||||||| |    |  | |||||||    |   || ||   || |    | | | |  \\---++++--++-+++---+-+--+-+-+--+--+---/  | | ||   | | ||||| |   |  |\n| | | || |||||  ||     |||||||| |    |  | |||||||    |   || ||   ||/+----+-+-+-+------++++-\\|\\-+++---+-+--+-+-+--+--+------+-+-++---+-+-+++++-+---/  |\n| | | || |||||  ||     |||||||| |    |  | |||||||/---+---++-++---++++----+-+-+-+------++++-++--+++---+-+--+-+-+--+--+------+-+-++->-+-+-+++++-+\\     |\n| | | || |||||  ||     |||||||| |    |  | ||||||||   |   || ||   ||||    |/+-+-+------++++-++--+++---+-+--+-+-+--+--+------+-+-++---+-+\\||||| ||     |\n| | | || |||||/-++-----++++++++-+----+--+-++++++++---+---++-++---++++-\\  ||| |/+------++++-++--+++---+-+--+-+-+--+--+------+-+-++---+-+++++++-++----\\|\n| | | || |||||| ||     |||||||| |    |  | ||||||||   |   || ||   |||| |  ||| |||      |||| ||  |||   | |  | | |  |  |      | | ||   | ||||||| ||    ||\n| | \\-++-++++++-++-----++++++++-+----+--+-++++++++---+---++-++---+++/ |  ||| |||      |||| ||  |||   | |  | | |  |  |      | | ||   | ||||||| ||    ||\n| |   || |||||| ||     |||||||\\-+----+--+-++++++++---+---++-++---+++--+--+++-+++------++++-++--+++---+-/  | | |  |  |      | | ||   | ||||||| ||    ||\n| |   || |||||| ||     |||||||  |    |  | \\+++++++---+---++-++---+++--+--+++-+++------++++-++--+++---+----+-+-+--+--+------+-+-++---+-+++++++-++----+/\n| |   || |||||| ||     |||||||  |    |  |  |||||||   |   || ||   |||  |  ||| |||      |||| ||  |||   |    | | |  |  |      | | ||   | ||||||| ||    | \n| |   || |||||| ||     |||||||  |    |  |  |||||||   |   || ||   |||  |  ||| ||| /----++++-++--+++---+----+-+-+--+--+------+-+-++--\\| ||||||| ||    | \n| |   || |||||| ||     |||||||  |    |  |  |||\\+++---+---++-++---+++--+--+++-/|| |    |||| ||  |||   |    | | |  |  |      | | ||  || ||||||| ||    | \n| |   || |||||| ||     |||||||  |    |  |  ||| |||   |   |\\-++---+++--+--+++--++-+----++++-++--+++---+----+-+-+--+--+------+-+-+/  || ||||||| ||    | \n| |   || |||||| ||     |||||||  |    |  |  ||| |||   |   |  ||   |||  |  |||  || |    |||| ||  |||  /+----+-+-+--+--+------+-+-+--\\|| ||||||| ||    | \n| |   || |||||| ||     |||||||  |    |  |  ||| |||   |   \\--++---+++--+--+++--++-+----++++-++--/||  ||    | | |  |  |  /---+-+-+--+++-+++++++-++\\   | \n| |   \\+-++++++-++-----+++++++--+----+--+--+++-+++---/      ||   |||  |/-+++--++-+----++++-++---++-\\||    | | |  |  |  |   | | \\--+++-+++/||| |||   | \n| |    | |||||| ||     |||||||  |    |  |  ||| |||          ||   |||  || |||  || |    v||| ||   || |||   /+-+-+--+--+--+---+-+\\   ||| ||| ||| |||   | \n| |    | |||||| ||     |||||||  |    |  \\--+++-+++----------++---+++--++-+++--++-+----++++-++---++-+++---++-+-/  |  |  |   | ||   ||| ||| ||| |||   | \n| \\----+-++++++-++-----/|\\++++--+----+-----+++-+++----------++---+/|  || |||  || |    ||||/++---++-+++---++-+---\\|  |  |   | ||   ||| ||| ||| |||   | \n|      | |||||| \\+------+-++++--+----/     ||| |||          ||   | |  || |||  || |    |||||||   || |||   || |   ||  |  |   | ||   ||| ||| ||| |||   | \n|      | ||||||  |      | ||||  |          ||| |||          ||   | |  || |||  || |    |||||||   || |||   || |   ||  |  |   | ||   ||| ||| ||| |||   | \n|      | ||||||  |    /-+-++++--+------\\   ||| ||v          ||   | |  || |||  || |    |||||||   |\\-+++---++-+---++--+--+---+-++---+++-+++-++/ |||   | \n|      | ||||||  |    | | ||||  |      |   ||| |||          ||   | |  || |||  || |    ||\\++++---+--+++---++-/   ||  |  |   | ||   ||| ||| ||  |||   | \n|      | ||||||  |    | | ||||  |      |   ||| |||          ||   | |  || |||  \\+-+----++-++++---+--+++---++-----++--+--+---+-++---+++-+++-++--+++---/ \n|      \\-++++++--+----+-+-++++--+------+---+++-+++----------++---+-+--++-+++---/ |    || ||||   |  |||   ||     ||  |  |   | ||   ||| ||| ||  |||     \n|        ||||||  |    | | ||||  |      |   ||\\-+++----------++---+-+--++-+++-----+----++-++++---+--+++---++-----++--+--+---/ \\+---+++-+++-++--/||     \n|     /--++++++--+----+-+-++++--+------+---++--+++\\         |\\---+-+--++-/||     |    || ||||   |  |||   \\+-----++--+--+------/   ||| ||| ||   ||     \n|     |  ||||||  |    | | ||||  |      |   ||  ||||         |    | |  ||  ||     |    || ||||   |  |||    |     ||  |  |          ||| ||| ||   ||     \n|     |  \\+++++--+----+-+-++++--+------+---++--++++---------+----+-+--++--++-----+----/| ||||   |  |||    \\-----++--+--+----------+++-+++-/|   ||     \n|     |/--+++++--+----+-+-++++--+------+---++-\\\\+++---------+----+-+--++--++-----+-----+-++++---+--+++----------++--/  |          ||| |||  |   ||     \n|     ||  |||||  |    | | ||||  \\------+---++-+-+++---------+----+-+--++--++-----+-----+-++++---+--++/          ||     |          ||| |||  |   ||     \n|     ||  |||||  |    | | ||||         |   || | |||         |    | |  ||  ||     |     | ||||   |  ||           ||     |          ||| |||  |   ||     \n|     ||  |||||  |    | | ||||         |   || | |||         |    | |  ||  ||     |     | ||||   |  ||/----------++-----+------\\   ||| |||  |   ||     \n|     ||  |||||  |    | | ||||         |   || | |||         |    | \\--++--++-----+-----+-++/|   |  |||          ||     |      |   ||| |||  |   ||     \n|/----++--+++++--+----+-+-++++---------+---++-+-+++---------+----+----++--++-----+\\    | \\+-+---+--+++----------++-----+------+---+++-/||  |   ||     \n||    ||  |||||  |    \\-+-++++---------/   || | |||         |    |    ||  ||     ||    |  | |   |  |||          ||     |      |   |||  ||  |   ||     \n||    ||  |||||  |      | ||||             || | |||         |    |    ||  ||     ||    |  | |   |  |||          ||     \\------+---+++--++--+---+/     \n||    ||  |||||  |      | ||\\+-------------/| | |||         |    |    ||  || /---++----+--+-+---+--+++---\\      ||            |   |||  ||  |   |      \n||    |\\--+++++--+------+-++-+--------------+-/ |||         |    |    ||  || |   ||    |  | |   |  |||   |      ||            |   |||  ||  |   |      \n||    |   |||||  |      | || |  /-----------+\\  |\\+---------+----+----++--++-+---++----+--+-+---+--+++---+------++------------+---+++--++--+---/      \n||    |   |\\+++--+------+-+/ |  |   /-------++--+-+--\\      |    |    ||  || |   ||    |  | |   |  |||   |      ||            |   |||  ||  |          \n||    |  /+-+++--+------+-+--+--+---+-------++--+-+--+------+----+----++--++-+---++----+--+-+---+--+++\\  |      ||            |   |||  ||  |          \n||    |  || |||  |      | |  |  |   |       ||  | |  |      |    |    ||  || |   ||    |  | |   |  ||\\+--+------++------------/   |||  |^  |          \n||    |  || |||  |      | |  \\--+---+-------++--+-+--+------+----/    ||  || |   ||    |  | |   |  || |  |      ||                |||  ||  |          \n||    |  || |||  |      | |     |   |       ||  | |  |      |         ||  || |   ||    |  | |   |  || |  |      ||                |||  ||  |          \n||    \\--++-+++--+------+-+-----+---+-------++--+-/  |      |         ||  || |   ||    \\--+-+---+--++-+--+------++----------------++/  ||  |          \n||       || |||  |      \\-+-----+---+-------++--/    |      |         ||  || |   ||       | |   |  || |  |      |\\----------------++---++->/          \n||       || ||\\--+--------+-----+---+-------++-------+------+-<-------/|  || |   ||       | \\---+--++-+--+------+-----------------++---+/             \n||       |\\-++---+--------+-----+---+-------++-------+------+----------+--+/ |   ||       |     |  || |  |      |                 ||   |              \n\\+-------+--++>--+--------+-----+---+-------++-------+------/          |  |  \\---++-------+-----+--++-+--/      |                 ||   |              \n |       |  ||   \\--------+-----+---+-------++-------+-----------------+--+------++-------+-----/  || |         |                 ||   |              \n |       |  \\+------------+-----+---+-------/|       |                 |  \\------++-------+--------++-+---------+-----------------++---/              \n |       |   \\------------/     \\---+--------/       |                 |         \\+-------+--------++-+---------+-----------------+/                  \n |       |                          |                |                 \\----------+-------+--------/| |         |                 |                   \n \\-------+--------------------------+----------------+----------------------------/       |         \\-+---------+-----------------/                   \n         \\--------------------------+----------------+------------------------------------+-----------/         |                                     \n                                    \\----------------/                                    \\---------------------/                                     ")


(deftest part-1
  (testing "example"
    (is (= [7 3] (solve-1 example))))
  (testing "input"
    (is (= [119 41] (solve-1 input)))))


(deftest part-2
  (testing "example"
    (is (= [4 6] (solve-2 example-2))))
  (testing "input"
    (is (= [136 45] (solve-2 input)))))

