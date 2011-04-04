SimpleRTM - Interactive Emacs mode for Remember The Milk
========================================================

&copy; 2011 Moritz Bunkus &lt;moritz@bunkus.org&gt;<br>
&copy; 2010 Seth Mason &lt;seth@sethmason.com&gt; (for slack-rtm.el)<br>
&copy; 2009 Friedrich Delgado Friedrichs (for rtm.el)

Overview
--------

This is an interactive "do everything right now" kind of interface to
Remember The Milk (RTM). Upon start it will query RTM for all of your
lists and incomplete tasks and show them. Each list can be expanded
and collapsed. Tasks can be acted upon with single key strokes.

You can mark tasks and act upon multiple tasks at once. However, only
marked tasks that are currently visible (meaning that the list they
belong to is expanded) are considered when taking any action. If no
visible task is maked then the task at point is acted upon.

For a complete list of supported actions please see the mode's help.

For more infomation see
[the Remember The Milk website](http://www.rememberthemilk.com).

Installation
------------

Add the following to your startup file:

    (add-to-list 'load-path "/path/to/simple-rtm/lisp")
    (autoload 'simple-rtm "simple-rtm" "Interactive mode for Remember The Milk" t)

Then start it with:

    (simple-rtm-mode)

Limitations
-----------

This mode will not sync with org-mode. I don't use org-mode, and this
package is tailored for immediate modification of tasks. Please don't
ask for offline/sync functionality or org-mode integration.  See the
original project SimpleRTM started from for a solution:
[slack-rtm](https://github.com/slackorama/slack-rtm)

Anything available from RTM's "settings" section will not be
supported. Meaning you will not be able to e.g. create lists with
SimpleRTM.

Known issues
------------

* Smart lists don't work yet.
* Setting the location doesn't work yet.

Planned features
----------------

* Make fewer calls to RTM after each action (parse and integrate each
  call's result instead of reloading all tasks).
* Show full task details in other buffer
* Show/edit/delete notes
* Add docstrings to all interactive functions

Bugs and issues
---------------

If you find bugs not addressed here please open an issue in
[SimpleRTM's issue tracker](https://github.com/mbunkus/simple-rtm/issues).
