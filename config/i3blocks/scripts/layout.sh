#!/usr/bin/env sh

if [ "$XDG_SESSION_DESKTOP" = "sway" ]
then
    export isSWAY="y"
else
    export isSWAY=""
fi

if [ -n "$isSWAY" ]
then
    get_layout() {
        swaymsg -t get_inputs  | jq -r '.[] | select(.identifier == "1:1:AT_Translated_Set_2_keyboard").xkb_active_layout_name' \
            | tr '[:lower:]' '[:upper:]' \
            | sed -e 's/\(..\).*/\1/g'
    }

toggle_layout() {
    layout="$(get_layout)"
    if [ "FR" = "$layout" ]
    then
        swaymsg input type:keyboard xkb_layout us
    else
        swaymsg input type:keyboard xkb_layout fr
    fi
}
else

    get_layout() {
        setxkbmap -query | grep layout | awk '$0=$2' | tr '[:lower:]' '[:upper:]'
    }
    toggle_layout() {
        layout="$(get_layout)"
        if [ "FR,US" = "$layout" ]
        then
            setxkbmap -layout us,fr -option 'grp:alt_shift_toggle'
        else
            setxkbmap -layout fr,us -option 'grp:alt_shift_toggle'
        fi
    }
fi


case "$BLOCK_BUTTON" in
    1) toggle_layout >/dev/null ; get_layout ;;
    *) get_layout ;;
esac
