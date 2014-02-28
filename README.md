pw
==

A simple command-line password handler written in haskell.

### Commands
| Command              | Shorthand     | Explanation |
| -------------------- | :-----------: | :---------- |
| `-add`               | `-a`          | Adds a password to a specified file with a specified `tag`, the password is either specified or generated |
| `-create`            | `-c`          | Creates a new specified file. |
| `-file path`         | `-f`          | Specifies the file, used by `-add`, `-create` and `-read`. |
| `-generate [length]` | `-g`, `-gen`  | Genrates a randmized password with a optionally specified length, deafaults to 10. |
| `-read [tag]`        | `-r`          | Reads a specified files passwords. If a `tag` is specified then only that passwords with that `tag` will be given, else it reads all the passwords. |
| `-password password` | `-p`, `-pw`   | Specifies a password to be used in the program. Used by `-add` |
| `-tag tag`           | `-t`          | Specifies a tag for a password. Used by `-add` |

### Examples

`pw -create -file file.pw` -- Creates a new file, named `file.pw`. Will ask for you to specify a master password for that file.

`pw -add -file file.pw -tag github -password 123abc` -- Adds the password 123abc with the tag github to the already created file `file.pw`

`pw -read github -file file.pw` -- Reads the passwords taged with github in the file `file.pw`

`pw -add -file file.pw -tag fb -gen 15 -read` -- Adds a newly generated password of length 15 to `file.pw` with the tag `fb` AND then reads that password.

`pw -create -file new_file.pw -add -tag twitter -gen 20 -read` --Creates the new file `new_file.pw` and adds a genereated password of length 20 and then reads that password.
