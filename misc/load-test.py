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
# Then the test can be run as many times as necessary.  Repeated redemptions
# are allowed since the same tokens are used on every run.
#
# Originally written for https://github.com/PrivateStorageio/PaymentServer/issues/60

from __future__ import division

from time import (
    time,
)

from json import (
    dumps,
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

PARALLELISM = 30

@inlineCallbacks
def redeem(client, index):
    times = []
    for i in range(16):
        before = time()
        response = yield client.post(
            url="http://127.0.0.1:8080/v1/redeem",
            data=dumps({
                "redeemVoucher": "aaa{}".format(index),
                "redeemTokens": ["foo-{}-{}".format(index, i)],
                "redeemCounter": i,
            }),
            headers={"content-type": "application/json"},
        )
        after = time()
        duration = int((after - before) * 1000)
        print("Request complete in {}ms".format(duration))
        body = yield readBody(response)
        assert response.code == 200, (response.code, body)
        times.append(duration)
    returnValue(times)


def mean(xs):
    return sum(xs) / len(xs)


def percentile(n, xs):
    return sorted(xs)[int(len(xs) / 100 * 95)]


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
    for result in (yield gatherResults(ds)):
        times.extend(result)

    print("min: {}".format(min(times)))
    print("max: {}".format(max(times)))
    print("mean: {}".format(mean(times)))
    print("median: {}".format(median(times)))
    print("95th: {}".format(percentile(95, times)))
