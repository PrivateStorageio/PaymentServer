# Run a command, enabling cache writes to cachix if possible.  The command is
# accepted as a variable number of positional arguments (like argv).
function cache_if_able() {
    # The `cachix watch-exec ...` does our cache population.  When it sees
    # something added to the store (I guess) it pushes it to the named cache.
    #
    # We can only *push* to it if we have a CACHIX_AUTH_TOKEN, though.
    # in-repo jobs will get this from CircleCI configuration but jobs from
    # forks may not.
    if [ -v CACHIX_AUTH_TOKEN ]; then
	cachix watch-exec "${CACHIX_NAME}" -- "$@"
    else
	# If we're building a from a forked repository then we're allowed to
	# not have the credentials (but it's also fine if the owner of the
	# fork supplied their own).
	#
	# https://circleci.com/docs/built-in-environment-variables/ says about
	# `CIRCLE_PR_REPONAME`:
	#
	#   The name of the GitHub or Bitbucket repository where the pull
	#   request was created. Only available on forked PRs.
	#
	# So if it is not set then we should have had credentials and we fail
	# if we get here.
	if [ -v CIRCLE_PR_REPONAME ]; then
	    "$@"
	else
	    echo "Required credentials (CACHIX_AUTH_TOKEN) are missing."
	    return 1
	fi
    fi
}
