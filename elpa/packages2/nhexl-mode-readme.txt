This package implements NHexl mode, a minor mode for editing files
in hex dump format.  The mode command is called `nhexl-mode'.

This minor mode implements similar functionality to `hexl-mode',
but using a different implementation technique, which makes it
usable as a "plain" minor mode.  It works on any buffer, and does
not mess with the undo boundary or with the major mode.

In theory it could also work just fine even on very large buffers,
although in practice it seems to make the display engine suffer.