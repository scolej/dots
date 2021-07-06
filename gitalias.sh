#!/bin/sh

export GIT_MERGE_AUTOEDIT=no

alias g='git'

alias ga='git add'
alias gau='git add -u'
alias gaus='git add -u && git status'
alias gaud='git add -u && git diff'
alias gauds='git add -u && git diff --staged'

alias gb='git branch'
alias gba='git branch -a'
alias gbr='git branch -r'

alias gco='git checkout'

alias gcamend='git commit --amend --no-edit --date=now'
alias gcom='git commit'
alias gcomm='git commit -m'
alias gcfix='git commit --fixup'

alias gd='git diff --patience'
alias gds='git diff --patience --staged'
alias gdmb='git diff $(git merge-base @{u} HEAD)'

alias gl='git log --oneline'
alias gll='git log --pretty --graph' # Log with all the info we have
alias gl9='gl -n9'
alias gla="g log --format='%h %<(15,trunc)%an %s'" # One-line log with the author as well

alias glmb='git log --oneline ^$(git merge-base @{u} HEAD) HEAD'

alias grc='git rebase --continue'
alias gri='git rebase -i --autosquash'
alias grio='git rebase -i --autosquash origin/$(git rev-parse --abbrev-ref HEAD)' # Rebase onto the same branch on origin
alias grik='git rebase --interactive --keep-base'

alias groot='cd "$(git rev-parse --show-toplevel)" ; echo "I am Groot"'

alias gru='git remote update'

alias gs='git status'
alias gsh='git show HEAD'

alias gtl='git tag -l'

alias gwl='git worktree list'

gads() {
    git add "$@"
    git diff --staged "$@"
}

gas() {
    git add "$@"
    git status
}

gbg() {
    args=
    while test -n "$1"; do
        args="$args -e $1" # I'm sure this will not work if any args have spaces.
        shift
    done
    git branch -a | grep -i $args
}

gitbranchbyauthor() {
    git for-each-ref --sort=authorname --format "%(authorname) %(refname)"
}

glup() {
    g='git --no-pager log --oneline --decorate'
    echo '--- they have ---'
    $g '..@{u}'
    echo '--- we have ---'
    $g '@{u}..'
}

gg() {
    t1=$(mktemp)
    git --no-pager -c color.ui=always status > "$t1" 2>&1 &
    p1=$!

    t2=$(mktemp)
    git --no-pager -c color.ui=always log --oneline --decorate -n 10 --graph > "$t2" 2>&1 &
    p2=$!

    t3=$(mktemp)
    echo 'we have' >> "$t3"
    git --no-pager -c color.ui=always log --oneline --decorate HEAD '^@{u}' >> "$t3" 2>&1 &
    p3=$!

    t4=$(mktemp)
    echo 'they have' >> "$t4"
    git --no-pager -c color.ui=always log --oneline --decorate ^HEAD '@{u}' >> "$t4" 2>&1 &
    p4=$!

    t5=$(mktemp)
    echo 'unstaged diff' >> "$t5"
    git --no-pager -c color.ui=always diff > "$t5" 2>&1 &
    p5=$!

    wait $p1 $p2 $p3 $p4 $p5 > /dev/null

    all=$(mktemp)
    { cat "$t1" ; echo ; cat "$t2" ; echo ; cat "$t3"; echo ; cat "$t4"; echo ; cat "$t5"; } >> "$all"

    clear
    less -SR "$all"
}

gsum() {
    ref="$1"

    t1=$(mktemp)
    git --no-pager -c color.ui=always show --name-only "$ref" > "$t1" 2>&1 &
    p1=$!

    t2=$(mktemp)
    git --no-pager -c color.ui=always diff "^$ref" "$ref" > "$t2" 2>&1 &
    p2=$!

    wait $p1 $p2 > /dev/null

    all=$(mktemp)
    { cat "$t1" ; echo ; cat "$t2" ; } >> "$all"

    clear
    less -SR "$all"
}

gdetachhead() {
    git checkout "$(git rev-parse HEAD)"
}
