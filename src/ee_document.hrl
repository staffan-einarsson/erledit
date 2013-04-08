-record(buffer_line, {num = 0, data = []}).
-record(caret, {line = 0, column = 0}).

-define(ASCII_TAB, 9).
-define(ASCII_LF, 10).
-define(ASCII_CR, 13).
