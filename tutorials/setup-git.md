# Git tutorial

The purpose of this tutorial is to help you set up a minimal working environment to create and push your solutions to the exercises.
It also aims to introduce you to some useful tools and resources that offer a smoother and integrated lab workflow.

In the following we assume that:
+ You are reading this guide from your own fork of the [ocaml-challenge](https://github.com/informatica-unica/ocaml-challenge). If you don't know how to fork a repository, see the useful [GitHub documentation](https://docs.github.com/en/get-started/quickstart/fork-a-repo#forking-a-repository);

+ You are working on a Linux machine. If you are a Windows user, the most straightforward way to run a Linux environment on your Windows system is the [Windows Subsystem for Linux](https://learn.microsoft.com/en-us/windows/wsl/about), which can be easily installed [from the Microsoft Store](https://apps.microsoft.com/detail/9PN20MSR04DW?hl=en-us&gl=IT);

+ You have the OCaml compiler installed: check out the [installation instructions for Linux](https://ocaml.org/docs/installing-ocaml#installation-on-unix-and-macos).

## 1. Install `git`

Your Linux distribution most likely comes with `git` preinstalled. You can check this by running:

```bash
git --version
```

If the previous command returns an error, install `git`:

```bash
sudo apt update
sudo apt install git
```

## 2. Link `git` to your GitHub account

To manage your online repositories from the command line you need the [GitHub CLI](https://github.com/cli/cli). Follow the [installation instructions](https://github.com/cli/cli) that suit your Linux distribution.

Once you have `gh` installed, authenticate by running:

```bash
gh auth login
```

and follow the on-screen procedure carefully.

Now your `git` installation is linked with your GitHub account, however `git` still doesn't know who you are. For this, run the following commands with username and email of your GitHub account as arguments.

```bash
git config --global user.name <your_username>
git config --global user.email <your@email.com>
```

## 3. Clone your fork

*Cloning* downloads a local copy of your fork of the repository on your disk. This is where you edit the code of the exercises using your favorite code editor (we recommend [Visual Studio Code](https://code.visualstudio.com/docs/setup/linux) together with the [OCaml Platform extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)).

In a directory of your choice, run the following command with your actual username (and your fork's name in case you named it something other than `ocaml-challenge`) in the URL argument: 

```bash
git clone https://github.com/your_username/ocaml-challenge
```

## 4. Commit a solution

When you're ready to upload a solution of an exercise to your fork, first run:

```bash
git commit
```

to record the changes you made to a local commit, then run:

```bash
git push
```

to transmit the new commit to your remote (i.e. online) fork.

## 5. Synchronize your fork with `informatica-unica/ocaml-challenge`

To synchronize your fork on your browser, look for the ["Sync fork"](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/syncing-a-fork) button in the GitHub page of your fork's repository. Note that syncing on the browser does not affect your local copy of the fork that you cloned earlier.

To synchronize your local copy of the fork with the most recent version of the lab repository, run:

```bash
git pull
```

This might not work if you have some pending changes not yet committed to your working tree. In this case you can temporarily store away the modified files with:

```bash
git stash
```

and restore them later on top of the newer commits using:

```bash
git stash apply
```

**Tip:** you can always append the `--help` option to any of the above git commands to fully explore their functionality. Also refer to the [Git Cheat Sheet](https://education.github.com/git-cheat-sheet-education.pdf) for more important commands.
