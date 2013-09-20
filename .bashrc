# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
alias ls="ls --color=auto"
alias l="ls -al"

LESS="-X"
export LESS

THREADS=12
export THREADS

export LD_LIBRARY_PATH=$HOME/lib

if [[ $HOSTNAME == "cam-02-unx" ]]; then
    PLAYPEN_DIR="/playpen/t-jastol"
    GHC_BUILD_DIR="/playpen/t-jastol/build"
    GHC_VALIDATE_DIR="/playpen/t-jastol/ghc-validate"
elif [[ $HOSTNAME == "cam-04-unx" ]]; then
    PLAYPEN_DIR="/64playpen/t-jastol"
    GHC_BUILD_DIR="/64playpen/t-jastol/build"
    GHC_VALIDATE_DIR="/64playpen/t-jastol/ghc-validate"
else
    PLAYPEN_DIR="/5playpen/t-jastol"
    GHC_BUILD_DIR="/5playpen/t-jastol/build"
    GHC_VALIDATE_DIR="/5playpen/t-jastol/ghc-validate"
fi

GHC_SANDBOX_DIR=`readlink $HOME/.ghc-sandbox | sed "s/\/active//"`
LLVM_SANDBOX_DIR=`readlink $HOME/.llvm-sandbox | sed "s/\/active//"`
export GHC_SANDBOX_DIR LLVM_SANDBOX_DIR

GHC_WORKING_DIR="/home/t-jastol/ghc-working"
GHC_TESTS_DIR="/home/t-jastol/tests"
export GHC_WORKING_DIR GHC_TESTS_DIR GHC_BUILD_DIR GHC_VALIDATE_DIR PLAYPEN_DIR

STARTCOLOR="\[\e[32m\]";
ENDCOLOR="\[\e[0m\]";

PS1="[$STARTCOLOR\u$ENDCOLOR@\h : \w] "

STARTCOLOR="\e[32m";
ENDCOLOR="\e[0m";
ERRORCOLOR="\e[31m";
export STARTCOLOR ENDCOLOR ERRORCOLOR

alias e="emacsclient -t"
alias t="task"
alias td="task ls project:dom"
alias tp="task ls project:praca"
alias tm="task ls project:msr"

stty -ixon

PATH="$HOME/bin:$GHC_SANDBOX_DIR/active/bin:$LLVM_SANDBOX_DIR/active/bin:$HOME/.cabal/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"
export PATH

function CompleteGHCSandbox () {
  local envs=`ls $GHC_SANDBOX_DIR | grep -v active`
  local word=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=(`compgen -W "$envs" -- "$word"`)
}

complete -F CompleteGHCSandbox ghc-sandbox

function CompleteLLVMSandbox () {
  local envs=`ls $LLVM_SANDBOX_DIR | grep -v active`
  local word=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=(`compgen -W "$envs" -- "$word"`)
}

complete -F CompleteLLVMSandbox llvm-sandbox

_ghc()
{
    local envs=`$GHC_SANDBOX_DIR/master/bin/ghc --show-options`
    # get the word currently being completed
    local cur=${COMP_WORDS[$COMP_CWORD]}

    # the resulting completions should be put into this array
    COMPREPLY=( $( compgen -W "$envs" -- $cur ) )
}

complete -F _ghc -o default ghc

umask 003
