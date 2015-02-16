Purpose is a package that introduces the concept of a "purpose" for
windows and buffers, and then helps you maintain a robust window
layout easily. Purpose is intended to help both regular users and
developers who want Emacs to have a more IDE-like behavior.

Note: Currently Purpose is supported for Emacs 24.4 and newer. The
main reason is because it uses `advice-add' and `advice-remove'.

Typical Usage (Regular User)
1. Turn Purpose on (`purpose-mode').
2. Configure which purposes you want your windows to have (see
   purpose-configuration.el).
3. Arrange your window layout as you want it to be. Any window which
   you want to dedicate to a specific purpose (so it won't be used
   for other purposes), you shuld dedicate with
   `purpose-toggle-window-purpose-dedicated'.
4. Purpose uses advice, so Emacs uses purpose-aware commands instead
   of the original commands when you need to change buffers. (e.g.
   `purpose-switch-buffer' instead of `switch-to-buffer'). This will
   open your buffers in the correct windows.
- To save your layout, or load a previously saved layout, use
   `purpose-save-window-layout', `purpose-load-window-layout',
   `purpose-save-frame-layout' and `purpose-load-frame-layout'. You
   can load a saved layout and skip phases 1 and 2, of course.

Important Features:
- Configurable: Configure how Purpose decides what's your buffer's
   purpose. Note that the window's purpose is determined by its
   buffer.
- Persistent Window Layout: You can save and load your window layout
   between sessions by using `purpose-save-window-layout',
   `purpose-load-window-layout', `purpose-save-frame-layout' and
   `purpose-load-frame-layout'.
- Purpose-Aware Buffer Switching: Purpose uses advices (overrides)
   `display-buffer-overriding-action' in order to make Emacs' buffer
   switching functions "purpose-aware".
- Developer-Friendly: Purpose has hooks and an API that should make
   it easy for developers to use it as a part of more sophisticated
   plugins. If it isn't, your input is welcome.

Developer Usage (informal API):
- `purpose-set-window-layout', `purpose-load-window-layout': use this
   to set a window layout that suits your plugin.  -
- `purpose-get-window-layout' or `purpose-save-window-layout': use
   this to save a layout so you can add it to your plugin later.
- Functions for changing frame layout (similar to window layout)
- `purpose-get-extra-window-params-function': use this if you want to
   save additional window parameters that make sense for your plugin,
   when `purpose-get-window-layout' is called.
- `purpose-set-window-properties-functions': use this hook if you
   want to set extra properties for new windows, when
   `purpose-set-window-layout' is called.
- `set-configuration', `add-configuration': use these to change the
   purpose configuration to suit your plugin's needs.
- `purpose-select-buffer-hook': use this if you want to run some
   code every time a buffer is selected.
- `without-purpose': use this macro if you need to ignore purposes
   while executing some piece of code.
- `without-purpose-command': use this macro to create a command that
   ignores purposes.

Installation:
Download Purpose's source files and put them in your `load-path'.
Purpoes is available from MELPA, so the best way to do this is
with Emacs' package manager.
Next, add these lines to your init file:
   (require 'purpose)
   (purpose-mode)
And that's all.
