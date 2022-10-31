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

Build using Nix::

  $ nix-build nix/ -A PaymentServer.components.exes.PaymentServer-exe -o exe

Or using Stack::

  $ stack build

Testing
-------

You can perform manual integration testing against Stripe.
First, run the server::

  $ ./exe/bin/PaymentServer-exe [arguments]

Or with stack::

  $ stack run -- [arguments]

Then report that payment has been received for a given voucher:

   $ stack run -- \
       PaymentServer-complete-payment \
       --voucher abcdefg \
       --server-url http://localhost:8081/ \
       --webhook-secret-path ../stripe.webhook-secret

The PaymentServer marks the voucher as paid in its database.
Then redeem the vouncher for tokens::

   $ curl \
     http://<youraddress>:8081/v1/redeem \
     -X POST \
     -H 'content-type: application/json' \
     --data '{ "redeemVoucher": "abcdefg", "redeemTokens":[]}'
