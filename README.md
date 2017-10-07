The source code for blocksqld.

config.txt
--------------
Set user to the user under which blocksqld will be executed
and a database password for the user in `config.txt`.


Postgresql stuff
----------------

Create a role with the same name under which `blocksqld` will be executed with:
```bash
sudo -u postgres createuser -s -e myDbUser
sudo -u postgres createdb myDatabasename
```
Then connect to the database:
```bash
psql -d myDatabasename
```
and set a password with postgres command `\password`.

Alternatively, myDbUser's password can be set with:
```bash
sudo -u postgres createuser -P -s -e myDbUser
```

Compile blocksqld
-------------------
To compile and execute:

```bash
stack build
stack exec blocksqld
```

If there are database schema changes, it may be necessary to run:

```bash
dropdb myDatabasename && createdb myDatabasename
```

before starting the application.

