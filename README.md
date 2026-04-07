## Advent of code 2025
Always fun to do these and figured I would get a bit of
milage out of my very early stage skills in scala.

[https://adventofcode.com/2025](https://adventofcode.com/2025)

Unfortunately it was changed to 12 days rather than 24
but perhaps that leaves more time to do other projects.

### How to run
Install scala-cli if you don't have it on your machine already.
```bash
brew install Virtuslab/scala-cli/scala-cli
```
Here using brew, but you can refer to official page for other [installations](https://scala-cli.virtuslab.org/install)

Clone the repo and add your puzzle inputs under `inputs/` named `day01.txt`, 
`day02.txt`, etc. (AoC asks users to refrain from sharing their inputs therefore
they are kept outside of the repo)

Then run scala-cli from the root of the repo, replacing `day01` with whichever day you want:
```bash
scala-cli run . -M day01.main
```
