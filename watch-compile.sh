#!/usr/bin/env bash

COLOR_OFF="\e[0m";
DIM="\e[2m";

LOCKNAME=$(cat /dev/urandom | tr -cd 'a-f0-9' | head -c 16);
LOCKFILE="/tmp/elm-lock-${LOCKNAME}"

function compile {
  elm make --docs docs.json --output /dev/null
  yarn elm-test #--fuzz 10000
}

function run {
  (
  flock 200

  clear;
  tput reset;

  echo -en "${DIM}";
  date -R;
  echo -en "${COLOR_OFF}";

  compile;

  ) 200>"${LOCKFILE}"
}

run;

inotifywait -mqr -e close_write --format '%w %e %f' src tests | while read DIR EVENT FILE; do
  run;
done;
