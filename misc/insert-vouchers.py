#!/usr/bin/env python3

# Description: Insert vouchers into the payment server database
#
# Usage: insert-vouchers.py < vouchers.csv | ssh root@payments.$DEPLOYMENT nix-shell \
#            -p sqlite --run '"sqlite3 /var/lib/zkapissuer-v2/vouchers.sqlite3"'
#
# Author: Jean-Paul Calderone
#
# Comment: It's too unbearable for me to commit to any repo or ever run again.
#          If we need this activity again I'll turn it into some kind of real
#          engineering task.
#
# Comment 2: Not to worry, I'll do it! ~ Flo

from sys import stdin
print('BEGIN IMMEDIATE TRANSACTION;')
for line in stdin:
    _, voucher = line.split(",")
    print('INSERT INTO [vouchers] ([name], [charge_id]) VALUES ("{}", "{}");'.format(
        voucher, "notify-me",
    ))
print('COMMIT;')

