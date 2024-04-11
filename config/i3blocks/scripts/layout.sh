#!/usr/bin/env sh
get_layout() {
    setxkbmap -query | grep layout | awk '$0=$2' | tr '[:lower:]' '[:upper:]'
}

toggle_layout() {
    if [ "FR,US" = "$(get_layout)" ]
    then
        setxkbmap -layout us,fr -option 'grp:alt_shift_toggle'
    else
        setxkbmap -layout fr,us -option 'grp:alt_shift_toggle'
    fi
}

case "$BLOCK_BUTTON" in
    1) toggle_layout ; get_layout ;;
    *) get_layout ;;
esac
