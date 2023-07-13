#!/bin/bash

ps aux | grep 'stack exec queue slave' | grep -v 'grep' | awk '{print $2}' | xargs -n 1 kill -9