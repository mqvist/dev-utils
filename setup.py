"""Setup for the devutils package."""

import setuptools


with open("README.md") as f:
    README = f.read()

setuptools.setup(
    author="Mika Qvist",
    author_email="mika.k.qvist@gmail.com",
    name="devutils",
    license="Apache",
    description=".",
    version="v0.0.1",
    long_description=README,
    url="https://github.com/shaypal5/chocobo",
    packages=setuptools.find_packages(),
    python_requires=">=3.5",
    install_requires=["pytest", "pyinotify"],
    scripts=["bin/runtests"],
)
