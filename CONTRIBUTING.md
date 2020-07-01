# Contribution guide

```sh
ℹ️ Copied from another repo, needs updating!
```

## Clone the repository

> NOTE: this project requires Python 3.6+

First, fork the project.

```sh
# Clone your fork into your local machine.
# This will create the `origin` remote pointing to your fork.
git clone git@github.com:<your-username>/cbee.git
cd cbee

# Add an `upstream` remote pointing to the original repository.
git remote add upstream git@github.com:bee-field/cbee.git
```

## Setting up your environment

Create and activate a new virtual environment.

```sh
python3 -m venv env
source env/bin/activate
```

> **Note**: Once you are all done, you can deactivate it by running `deactivate`.

Install the package in "editable" (development) mode.

```sh
pip install -r requirements.txt
pip install -e .
```

## Modifying the code

First, make sure you're on the latest version.

```sh
git checkout master
git pull --rebase upstream master
git push
```

Now, create a new branch for your changes.
Try to use a short and descriptive name for your changes.

```sh
# Create a new branch and change to it.
git checkout -b your-branch
```

You can now modify whatever you want.

## Creating a Pull Request

After all the tests pass, you'll have to create a "Pull Request" with your changes.
You can create a single commit with all the changes.

> **Note**: You can check your changes with `git status`.

```sh
# Create a commit and push it to your fork's branch.
git add .
git commit -m 'One line description of your changes'
git push origin your-branch
```

Then you can follow the link in your terminal, or navigate to
[md2ipynb](https://github.com/davidcavazos/md2ipynb),
to create a "Pull Request".

> If you need to add further modifications, you'll have to:
>
> ```sh
> git add .
> git commit -m 'One line description of further changes'
> git push origin your-branch
> ```
>
> Afterwards, it will reflect automatically on the Pull Request.

Once everything is okay, it can be merged.

## Publishing a new version

Make sure you have some more tools installed.

```sh
pip install -U twine wheel
```

Change the version on `setup.py`.
Then, generate the distribution archives.

```sh
# Make sure there are no previous builds.
rm -rf dist/

# Build the distribution package.
python setup.py sdist bdist_wheel

# Check any problems with the README.
twine check dist/*
```

> **Note**: It is *highly recommended* to publish to `TestPyPI` before publishing to `PyPI`.
>
> ```sh
> # To upload to TestPyI.
> twine upload --repository-url https://test.pypi.org/legacy/ dist/*
>
> # To install from TestPyPI.
> pip install --index-url https://test.pypi.org/simple/ --no-deps md2ipynb
> ```
>
> If everything is working correctly, go ahead and publish to `PyPI`.

To upload to `PyPI`.

```sh
twine upload dist/*
```
