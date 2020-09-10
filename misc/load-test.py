#! /usr/bin/env nix-shell
#! nix-shell -i python -p "python.withPackages (ps: [ ps.treq ])"

# Apply a very minimal load to a PaymentServer via the redemption endpoint.
#
# This requires a PaymentServer run something like:
#
#   $ ./result/bin/PaymentServer-exe --issuer Trivial --http-port 8080 --stripe-key-path ../stripe.secret --database SQLite3 --database-path ./vouchers.db
#
# It also requires some vouchers to be marked as paid somehow.  The vouchers
# are aaa0 through aaaN where N is the PARALLELISM you select.  You can
# accomplish this by:
#
#   $ for n in $(seq 0 30); do sqlite3 vouchers.db "insert into vouchers (name) values ('aaa$n')"; done
#
# Then the test can be run as many times as necessary.
#
# The `redeemed` table must be cleared before each test run.  Random tokens
# are generated for each test run and the server will reject tokens from a new
# run if tokens from an old run are still present.
#
#  $ sqlite3 vouchers.db "delete from redeemed"
#
# Originally written for https://github.com/PrivateStorageio/PaymentServer/issues/60

from __future__ import division

from os import (
    urandom,
)

from base64 import (
    b64encode,
)

from time import (
    time,
)

from json import (
    dumps,
    loads,
)

from treq.client import (
    HTTPClient,
)

from twisted.web.client import (
    Agent,
    readBody,
)
from twisted.internet.task import (
    react,
)
from twisted.internet.defer import (
    inlineCallbacks,
    gatherResults,
    returnValue,
)

PARALLELISM = 50
ITERATIONS = 16
NUM_TOKENS = 5000


def a_random_token():
    return b64encode(urandom(32))


def tokens_for_voucher(key, cache={}):
    if key not in cache:
        print("Generating tokens for {}".format(key))
        cache[key] = list(
            a_random_token()
            for _
            in range(NUM_TOKENS)
        )
    else:
        print("Using cached tokens for {}".format(key))
    return cache[key]


@inlineCallbacks
def redeem_with_retry(client, data, headers):
    """
    Attempt a redemption.  Retry if it fails.

    :return: A ``Deferred`` that fires with (duration of successful request,
        number of failed requests).
    """
    errors = 0
    while True:
        before = time()
        response = yield client.post(
            url="http://127.0.0.1:8080/v1/redeem",
            data=data,
            headers=headers,
        )
        after = time()
        duration = int((after - before) * 1000)
        body = yield readBody(response)
        if response.code == 200:
            print("Request complete in {}ms".format(duration))
            returnValue((duration, errors))

        errors += 1
        try:
            reason = loads(body)["reason"]
        except ValueError:
            reason = body

        print("Request failed: {} {}".format(response.code, reason))


@inlineCallbacks
def redeem(client, index):
    times = []
    total_errors = 0
    voucher = "aaa{}".format(index)
    for i in range(ITERATIONS):
        tokens = tokens_for_voucher((voucher, i))
        duration, errors = yield redeem_with_retry(
            client,
            data=dumps({
                "redeemVoucher": voucher,
                "redeemTokens": tokens,
                "redeemCounter": i,
            }),
            headers={"content-type": "application/json"},
        )
        times.append(duration)
        total_errors += errors
    returnValue((times, total_errors))


def mean(xs):
    return sum(xs) / len(xs)


def percentile(n, xs):
    return sorted(xs)[int(len(xs) / 100 * n)]


def median(xs):
    return percentile(50, xs)


@react
@inlineCallbacks
def main(reactor):
    client = HTTPClient(Agent(reactor))

    ds = list(
        redeem(client, i)
        for i
        in range(PARALLELISM)
    )

    times = []
    total_errors = 0
    for (result, errors) in (yield gatherResults(ds)):
        times.extend(result)
        total_errors += errors

    print("min: {}".format(min(times)))
    print("max: {}".format(max(times)))
    print("mean: {}".format(mean(times)))
    print("median: {}".format(median(times)))
    print("95th: {}".format(percentile(95, times)))
    print("errors: {}".format(total_errors))
    print("error rate: {}".format(
        total_errors / (total_errors + PARALLELISM * ITERATIONS),
    ))
