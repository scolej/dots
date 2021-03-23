#!/bin/sh

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

alias gl='git log --oneline -n 15'
alias gll='git log --pretty'
alias glmb='git log --oneline ^$(git merge-base @{u} HEAD) HEAD'

alias grc='git rebase --continue'
alias gri='git rebase -i --autostash --autosquash'
alias grik='git rebase --interactive --keep-base'

alias groot='cd "$(git rev-parse --show-toplevel)" ; echo "I am Groot"'

alias gru='git remote update'

alias gs='git status'
alias gsh='git show HEAD'

alias gtl='git tag -l'

alias gwl='git worktree list'

function gads {
    git add $@
    git diff --staged
}

function gas {
    git add $@
    git status
}

function gbg {
    git branch -a | grep -i $@
}

function git-branch-by-author {
    git for-each-ref --sort=authorname --format "%(authorname) %(refname)"
}

function glup {
    echo '--- they have ---'
    git log --oneline ..@{u}
    echo '--- we have ---'
    git log --oneline @{u}..
}

function gstat {
    t1=$(mktemp)
    t2=$(mktemp)
    t3=$(mktemp)
    git status --color=always &> "$t1"
    git log --oneline --decorate -n 10 &> "$t2"
    git diff &> "$t3"
    cat "$t1" "$t2" "$t3" | less -SR
}
