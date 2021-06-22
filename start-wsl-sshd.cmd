@echo off
netsh interface portproxy add v4tov4 listenport=2222 listenaddress=0.0.0.0 connectport=22 connectaddress=localhost
wsl -e sudo service ssh start
wsl -e sudo mkdir -p /var/run/screen
wsl -e sudo chmod 777 /var/run/screen
