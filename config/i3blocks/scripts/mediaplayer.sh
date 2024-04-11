#!/usr/bin/env sh

PLAYER="--player=rhythmbox"
t_max=17
a_max=17

pad () {
    [ "$#" -gt 1 ] && [ -n "$2" ] && printf "%$2.${2#-}s" "$1"
}

print_display() {
    output=''
    artist=$(playerctl $PLAYER metadata artist | iconv -f UTF-8 -t ASCII//TRANSLIT)
    title=$(playerctl $PLAYER metadata title | iconv -f UTF-8 -t ASCII//TRANSLIT)
    if [ "Unknown" != "$artist" ]
    then
        if [ "${#title}" -lt "$t_max" ]
        then
            # TITLE is lower than max we can give all space to artist
            output="$(pad "$title - $artist" -30)"
        else
            # Title is too big, can artist gi us space ?
            if [ "${#artist}" -lt "$a_max" ]
            then
                # yes, add the diff to t_max
                a_diff=$((a_max - ${#artist}))
                t_new=$((t_max + a_diff))
                output="$(pad "$title" $t_new) - $artist"
            else
                # Not enough space for both
                output="$(pad "$(playerctl $PLAYER metadata title | iconv -f UTF-8 -t ASCII//TRANSLIT)" 17)"
                output="$output - $(pad "$artist" -10)"
            fi
        fi
    else
        output=$(pad "$(playerctl $PLAYER metadata title | iconv -f UTF-8 -t ASCII//TRANSLIT)" -30)
    fi
    echo "$output"
}

case "$BLOCK_BUTTON" in
    1) playerctl $PLAYER play-pause ; pad "$(playerctl status)" -30 ;;
    2) pad "Previous" -30 ; playerctl $PLAYER previous ;;
    3) pad "Next" -30 ; playerctl $PLAYER next ;;
    *) print_display ;;
esac
