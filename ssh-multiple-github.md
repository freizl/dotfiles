<http://mherman.org/blog/2013/09/16/managing-multiple-github-accounts>


- ssh config `.ssh/config`

```

Host personal
   HostName github.com
   User git
   IdentityFile ~/.ssh/id_rsa_freizl

Host work
   HostName github.com
   User git
   IdentityFile ~/.ssh/id_rsa


```

- set up

```

#!/bin/sh

ssh-add -D
ssh-add id_rsa
ssh-add id_rsa_freizl
ssh-add -l
ssh -T personal
ssh -T work

```
