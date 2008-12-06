from Pymacs import lisp
import unicodedata

def insert_char(name):
    try:
        c = unicodedata.lookup(name)
        lisp.insert(ord(c))
    except KeyError:
        lisp.message("Unknown character name.")

insert_char.interaction = 'sCharacter Name: '
