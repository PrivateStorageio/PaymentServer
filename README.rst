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

Then configure Stripe with a `webhook`_ pointing at the server and receiving the *charge.successful* event.
Configure Stripe with ``http://<youraddress>:8081/v1/stripe/webhook``.

Then create a testing charge using Stripe::

   $ STRIPE_SECRET_KEY=sk_test_...
   $ curl \
     https://api.stripe.com/v1/charges   \
     -u ${STRIPE_SECRET_KEY}:   \
     -d amount=999   \
     -d currency=usd   \
     -d source=tok_visa   \
     -d 'metadata[Voucher]=abcdefghijk'

This results in Stripe making a callback to the PaymentServer with the charge details.
The PaymentServer marks the voucher as paid in its database.

.. _webhook: https://stripe.com/docs/webhooks/setup#configure-webhook-settings
