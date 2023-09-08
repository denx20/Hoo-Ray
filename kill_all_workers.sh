#!/bin/bash
# A command that kills all the running workers on a machine

ps aux | grep 'stack exec queue slave' | grep -v 'grep' | awk '{print $2}' | xargs -n 1 kill -9