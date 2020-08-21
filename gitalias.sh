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
alias gbd='git branch -a | grep -i develop'
alias gcamend='git commit --amend --no-edit --date=now'
alias gco='git checkout'
alias gcom='git commit'
alias gcomm='git commit -m'
alias gcfix='git commit --fixup'
alias gd='git diff'
alias gds='git diff --staged'
alias gdu='git diff $(git merge-base @{u} HEAD)'
alias gl='git log --oneline'
alias glu='git log --oneline @{u}..'
alias gr='git reset'
alias grc='git rebase --continue'
alias gri='git rebase -i --autostash --autosquash'
alias groot='cd "$(git rev-parse --show-toplevel)" ; echo "I am Groot"'
alias grrr='git rebase'
alias gru='git remote update'
alias grup='git remote update -p'
alias gs='git status'

function gads() {
    git add $@
    git diff --staged
}

function gas() {
    git add $@
    git status
}
