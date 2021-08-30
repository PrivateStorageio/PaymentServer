PaymentServer
=============

PaymentServer maintains the state of a voucher database with respect to payments.
It receives payment notifications from payment processors and notes this in the database.

Currently, Stripe is supported.

Building
--------

Get all the build dependencies with nix::

  $ nix-shell PrivateStorageio/shell.nix   # Might be needed depending on your system, see #88
  $ nix-shell PaymentServer/shell.nix

Build using Stack::

  $ stack build

Testing
-------

You can perform manual integration testing against Stripe.
First, run the server::

  $ stack run

Then create a testing charge::

   $ curl \
     http://<youraddress>:8081/v1/stripe/charge \
     -X POST \
     -H 'content-type: application/json' \
     --data '{ "token":"tok_visa", "voucher":"abcdefg", "amount":"650", "currency":"USD" }'

The PaymentServer marks the voucher as paid in its database.
Then redeem the vouncher for tokens::

   $ curl \
     http://<youraddress>:8081/v1/redeem \
     -X POST \
     -H 'content-type: application/json' \
     --data '{ "redeemVoucher": "abcdefg", "redeemTokens":[]}'
