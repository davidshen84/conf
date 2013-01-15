#!/bin/bash

EMAILPATH="/var/tmp/emerge-sync.email"

# create the email
echo "From: davidshen84@qq.com
Subject: emerge-sync
" > "$EMAILPATH"

# sync. portage and layman
eix-sync >> "$EMAILPATH"
layman -S >> "$EMAILPATH"

# generate package update list
emerge -puvND world >> "$EMAILPATH"

# send email to me
sendmail root < "$EMAILPATH"
