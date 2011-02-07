#### Install Instructions

1) Clone my git repository.
2) Run "make" with elevated privs so that it can install.
3) Laugh at how much work you've saved compared to the real way of getting camp.

#### Why?


Here's my story about my trying to actually install and use Camp.

1) Google for Camp, or Camp+Haskell. Get nothing useful in the first page or so.

2) Finally find the official page on it.

3) Stumble around the page until you find the link to their source code repository.

4) Install darcs on my computer (I normally use git instead).

5) Run `darcs get http://code.haskell.org/camp/devel/src/ camp`

6) Watch it 404. Swear a bit.

7) Stumble around the luckily HTTP-indexed filesystem on code.haskell.org until finding the right folder.

8) `darcs get http://code.haskell.org/camp/src/ camp`

9) Get darcs warnings about non-cached repositories and CRC check fails.

10) Run darcs CRC fixing program, (`darcs gzcrcs`) which doesn't find any issues. Swear a bit more.

11) Look at the camp README.txt. See that it contains no information at all except for already well-named sub-directories are. That's alright, we're smart people, we can figure it out ourselves.

12) cd into the camp-bin directory, and attempt to run `runhaskell Setup configure`.

13) Watch it fail because it has dependencies on other parts of the project.

14) Try to build other parts of the project, and have that mostly fail because each of the parts has dependencies on other parts of the project. Swear a bit.

15) Write my own makefile to deal with the dependencies.

16) Start a git repo in order to put Makefile and some other utilities on the web, along with some changes to camp.

17) Go to sleep for a while.

18) Try to pull in the darcs repo into git, using someone else's git repo. 

19) Realize that while you were busy sleeping, the code server on haskell.org has gone down, so you can't even retrieve the code again to test anything. Swear a bit.

20) Wait a few days for code.haskell.org to be up again.

21) Make this repository.
