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

Stripe Integration
------------------

PaymentServer listens for Stripe events at a "webhook" endpoint.
The endpoint is at ``/v1/stripe/webhook``.
It handles only ``checkout.session.completed`` events.
These events must include a voucher in the ``client_reference_id`` field.
A voucher so referenced will be marked as paid when this event is processed.

The webhook must be correctly configured in the associated Stripe account.
One way to configure it is with a request like::

  curl \
    https://api.stripe.com/v1/webhook_endpoints \
    -u sk_test_yourkey: \
    -d url="https://serveraddress/v1/stripe/webhook" \
    -d "enabled_events[]"="checkout.session.completed"
