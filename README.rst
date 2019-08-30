PaymentServer
=============

Testing
-------

You can run the PaymentServer automated test suite using stack::

  $ stack test

You may also want to perform manual integration testing against Stripe.
First, run the server::

  $ stack run

Then configure Stripe with a webhook pointing at the server.
The hook lives at ``/v1/stripe/webhook`` so configure Stripe with,
eg, ``http://youraddress:8081/v1/stripe/webhook``.

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
