#!/bin/sh

BUILTIN_KBD_ID=$(xinput --list --id-only 'AT Translated Set 2 keyboard')

# swap CTRL and ALT
# make "print scrren" RWIN
xkbcomp -i $BUILTIN_KBD_ID - $DISPLAY <<EOF
xkb_keymap {
    xkb_keycodes  {
        include "evdev+aliases(qwerty)"

        <LALT> = 37;
        <LCTL> = 64;
        
        <RCTL> = 108;
        <RALT> = 105;

        <INS>  = 255;

        alias <ALGR> = <RALT>;
    };
    xkb_types     { include "complete" };
    xkb_compat    {
        include "complete"
        indicator "Caps Lock" { groups = 2; };
    };
    xkb_symbols   { include "pc+us+ru:2+inet(evdev)+capslock(grouplock)" };
    xkb_geometry  { include "pc(pc105)" };
};
EOF

MS_KBD_ID=$(xinput --list --id-only 'Microsoft Microsoft® 2.4GHz Transceiver v9.0')

if test $? -eq 0
then
    # swap CTRL and ALT
    # make "menu" RWIN
    xkbcomp -i $MS_KBD_ID - $DISPLAY <<EOF
    xkb_keymap {
        xkb_keycodes  {
            include "evdev+aliases(qwerty)"

            <LALT> = 37;
            <LCTL> = 64;

            <RCTL> = 108;
            <RALT> = 105;

            <RWIN> = 135;

            alias <ALGR> = <RALT>;
        };
        xkb_types     { include "complete" };
        xkb_compat    {
            include "complete"
            indicator "Caps Lock" { groups = 2; };
        };
        xkb_symbols   { include "pc+us+ru:2+inet(evdev)+capslock(grouplock)" };
        xkb_geometry  { include "pc(pc105)" };
    };
EOF

fi
