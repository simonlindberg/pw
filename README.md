pw
==

A simple command-line password handler written in haskell.

### Todo

* not encrypt the tag for faster lookup.
* more options for `-generate`?
* ability to choose encryption?

### Notes

* The passwords are encrypted with AES in OFB mode. Where the `key` and `iv` are created with the master passphrase
* The filehandling tests won't run on Windows, but I think it works.

### Commands
| Command              | Shorthand     | Explanation |
| -------------------- | :-----------: | :---------- |
| `--add`              | `-a`          | Adds a password to a specified file with a specified `tag`, the password is either specified or generated. |
| `--create`           | `-c`          | Creates a new specified file. |
| `--file=FILE`        | `-f FILE`     | Specifies the file. |
| `--generate[=LENGTH]` or `--gen[=LENGTH]` | `-g[LENGTH]`  | Generates a random password, default length 10. |
| `--read`             | `-r`        | Reads passwords from a specified file. If a `-t tag` is given it will only read that otherwise it reads all passwords. |
| `--password=PASSWORD` or `--pw=PASSWORD` | `-p PASSWORD` | Specifies a password to be used in the program. |
| `--tag=TAG`          | `-t TAG`          | Specifies a tag for the password. |


### Examples
| Command | Explanation |
| :-------| :-----------|
| `pw --create --file=file.pw` | Creates a new file, named `file.pw`. Will ask for you to specify a master passphrase for that file. |
| `pw --add --file=file.pw --tag=github --password=123abc` | Adds the password 123abc with the tag github to the already created file `file.pw` |
|`pw --read --file=file.pw` | Reads all the passwords in the file `file.pw` |
|`pw --add --file=file.pw --tag=fb --gen=15 --read` | Adds a newly generated password of length 15 to `file.pw` with the tag `fb` AND then reads that password.|
|`pw --create --file=new_file.pw --add --tag=twitter --gen=20 --read` | Creates the new file `new_file.pw` and adds a genereated password of length 20 with the tag `twitter` and then reads that password.|
| `pw -carg20 -f new_file.pw -t twitter` | Does the same thing as the one above. Creates file, adds password, reads password.|
