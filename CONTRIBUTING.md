# Contributing

**🎉 First off, thanks for contributing! 🎉**

- [✨ Types of contributions](#types-of-contributions)
- [📤 Pull Requests](#pull-requests)
- [🚀 Development Workflow](#development-workflow)
- [🔀 Release process](#release-process)
- [🔬 Testing](#testing)
- [🎨 Code style](#code-style)
- [📄 Contributor license agreement](#contributor-license-agreement)

## ✨ Types of contributions

We welcome all types of contributions, including bug fixes, feature enhancements,
bug reports, documentation, graphics, and many others.  You might consider contributing by:

- Report a bug or request a new feature in our [issue tracker](https://github.com/NCEAS/salmon-viz/issues)
- Fix a bug and contribute the code with a Pull Request
- Write or edit some documentation
- Sharing helpful tips or FAQ-type answers to users or future contributors
- Create screenshots or tutorials of features of MetacatUI
- Answer questions on DataONE Discussions
- ...

This is an open source project, and we welcome full
participation in the project.  Contributions are reviewed and suggestions are
made to increase value to the community.  We strive to
incorporate code, documentation, and other useful contributions quickly and
efficiently while maintaining a high-quality software product.

## 📤 Pull Requests
We use the pull-request model for contributions. See [GitHub's help on pull-requests](https://help.github.com/articles/about-pull-requests/).

In short:

- add an [issue](https://github.com/NCEAS/salmon-viz/issues) describing your planned changes, or add a comment to an existing issue;
- on GitHub, fork the [repository](https://github.com/NCEAS/salmon-viz)
- on your computer, clone your forked copy of the repository
- base your work on the `develop` branch and commit your changes
- push your branch to your forked repository, and submit a pull-request
- our team will be notified of your Pull Request and will review your changes
- our team may request changes before we will approve the Pull Request, or we will make them for you
- once the code is reviewed, our team will merge in your changes to `develop` for the next planned release

## 🚀 Development Workflow

Development is managed through the git repository at https://github.com/NCEAS/salmon-viz.  The repository is organized into several branches, each with a specific purpose.  

**main**. The `main` branch represents the stable branch that is constantly maintained with the current release.  It should generally be safe to install and use the `main` branch the same way as binary releases. The version number in all configuration files and the README on the `main` branch follows [semantic versioning](https://semver.org/) and should always be set to the current stable release, for example `2.8.5`.

**develop**. Development takes place on a single branch for integrated development and testing of the set of features
targeting the next release. Commits should only be pushed to this branch once they are ready to be deployed to
production immediately after being pushed. This keeps the `develop` branch in a state of readiness for the next release.
Any unreleased code changes on the `develop` branch represent changes that have been tested and staged for the next 
release. 
The tip of the `develop` branch always represents the set of features that are awaiting the next release. The develop
branch represents the opportunity to integrate changes from multiple features for integrated testing before release.

Version numbers on the `develop` branch represent either the planned next release number (e.g., `2.9.0`), or the planned next release number with a `beta` designator or release candidate `rc` designator appended as appropriate.  For example, `2.8.6-beta1` or `2.9.0-rc1`.

**feature**. To isolate development on a specific set of capabilities, especially if it may be disruptive to other 
developers working on the `develop` branch, feature branches should be created.

Feature branches are named as `feature-` + `{issue}` +  `-{short-description}`, with `{issue}` being the GitHub issue number related to that new feature. e.g. `feature-23-refactor-storage`.

All `feature-*` branches should be frequently merged with changes from `develop` to
ensure that the branch stays up to date with other features that have
been tested and are awaiting release.  Thus, each `feature-*` branch can be tested on its own before it is merged with other features on develop, and afterwards as well. Once a feature is complete and ready for full integration testing, it is generally merged into the `develop` branch after review through a pull request.

**bugfix**. A final branch type are `bugfix` branches, which work the same as feature branches, but fix bugs rather than adding new functionality. Sometimes it is hard to distinguish features from bug fixes, so some repositories may choose to use `feature` branches for both types of change. Bugfix branches are named similarly, following the pattern: `bugfix-` + `{issue}` +  `-{short-description}`, with `{issue}` being the GitHub issue number related to that bug. e.g. `bugfix-83-fix-name-display`.

### Development flow overview

```mermaid
%%{init: {  'theme': 'base', 
            'gitGraph': {
                'rotateCommitLabel': false,
                'showCommitLabel': false
            },            
            'themeVariables': {
              'commitLabelColor': '#ffffffff',
              'commitLabelBackground': '#000000'
            }
}}%%
gitGraph
    commit id: "1" tag: "v1.0.0"
    branch develop
    checkout develop
    commit id: "2"
    branch feature-A
    commit id: "3"
    commit id: "4"
    checkout develop
    merge feature-A id: "5"
    commit id: "6"
    commit id: "7"
    branch feature-B
    commit id: "8"
    commit id: "9"
    checkout develop
    merge feature-B  id: "10" type: NORMAL
    checkout main
    merge develop id: "11" tag: "v1.1.0"
```

## 🔀 Release process

1. Our release process starts with integration testing in a `develop` branch. Once all
changes that are desired in a release are merged into the `develop` branch, we run
the full set of tests on a clean checkout of the `develop` branch.
2. After testing, the `develop` branch is merged to main, and the `main` branch is tagged with
the new version number (e.g. `2.11.2`). At this point, the tip of the `main` branch will 
reflect the new release and the `develop` branch can be fast-forwarded to sync with `main` to 
start work on the next release.
3. Releases can be downloaded from the [GitHub releases page](https://github.com/NCEAS/salmon-viz/releases).

## 🔬 Testing

**Unit and integration tests**. We maintain a full suite of tests in the `tests` subdirectory.
Any new code developed should include a robust set of tests for each public
method, as well as integration tests from new feature sets.  Tests should fully
exercise the feature to ensure that it responds correctly to both good data inputs
and various classes of corrupt or bad data.  All tests should pass before submitting a PR
or merging to `develop`.

Tests are automatically run via GitHub Actions. Check the root `README.md` file
for this GitHub Actions status badge and make sure it says "Passing":

## 🎨 Code style

Code should be written to professional standards to enable clean, well-documented,
readable, and maintainable software.  While there has been significant variability
in the coding styles applied historically, new contributions should strive for
clean code formatting.  We generally follow PEP8 guidelines for Python code formatting,
typically enforced through the `black` code formatting package.

## 📄 Contributor license agreement

In order to clarify the intellectual property license
granted with Contributions from any person or entity, you agree to
a Contributor License Agreement ("CLA") with the Regents of the University of
California (hereafter, the "Regents").

1. Definitions.
   "You" (or "Your") shall mean the copyright owner or legal entity
   authorized by the copyright owner that is making this Agreement
   with the Regents. For legal entities, the entity making a
   Contribution and all other entities that control, are controlled
   by, or are under common control with that entity are considered to
   be a single Contributor. For the purposes of this definition,
   "control" means (i) the power, direct or indirect, to cause the
   direction or management of such entity, whether by contract or
   otherwise, or (ii) ownership of fifty percent (50%) or more of the
   outstanding shares, or (iii) beneficial ownership of such entity.
   "Contribution" shall mean any original work of authorship,
   including any modifications or additions to an existing work, that
   is intentionally submitted by You to the Regents for inclusion
   in, or documentation of, any of the products owned or managed by
   the Regents (the "Work"). For the purposes of this definition,
   "submitted" means any form of electronic, verbal, or written
   communication sent to the Regents or its representatives,
   including but not limited to communication on electronic mailing
   lists, source code control systems, and issue tracking systems that
   are managed by, or on behalf of, the Regents for the purpose of
   discussing and improving the Work, but excluding communication that
   is conspicuously marked or otherwise designated in writing by You
   as "Not a Contribution."
2. Grant of Copyright License. Subject to the terms and conditions of
   this Agreement, You hereby grant to the Regents and to
   recipients of software distributed by the Regents a perpetual,
   worldwide, non-exclusive, no-charge, royalty-free, irrevocable
   copyright license to reproduce, prepare derivative works of,
   publicly display, publicly perform, sublicense, and distribute Your
   Contributions and such derivative works.
3. Grant of Patent License. Subject to the terms and conditions of
   this Agreement, You hereby grant to the Regents and to
   recipients of software distributed by the Regents a perpetual,
   worldwide, non-exclusive, no-charge, royalty-free, irrevocable
   (except as stated in this section) patent license to make, have
   made, use, offer to sell, sell, import, and otherwise transfer the
   Work, where such license applies only to those patent claims
   licensable by You that are necessarily infringed by Your
   Contribution(s) alone or by combination of Your Contribution(s)
   with the Work to which such Contribution(s) was submitted. If any
   entity institutes patent litigation against You or any other entity
   (including a cross-claim or counterclaim in a lawsuit) alleging
   that your Contribution, or the Work to which you have contributed,
   constitutes direct or contributory patent infringement, then any
   patent licenses granted to that entity under this Agreement for
   that Contribution or Work shall terminate as of the date such
   litigation is filed.
4. You represent that you are legally entitled to grant the above
   license. If your employer(s) has rights to intellectual property
   that you create that includes your Contributions, you represent
   that you have received permission to make Contributions on behalf
   of that employer, that your employer has waived such rights for
   your Contributions to the Regents, or that your employer has
   executed a separate Corporate CLA with the Regents.
5. You represent that each of Your Contributions is Your original
   creation (see section 7 for submissions on behalf of others).  You
   represent that Your Contribution submissions include complete
   details of any third-party license or other restriction (including,
   but not limited to, related patents and trademarks) of which you
   are personally aware and which are associated with any part of Your
   Contributions.
