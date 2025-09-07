from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="trexselector",
    version="0.6.15",
    author="Python port by Arnau Vilella",
    author_email="avp@connect.ust.hk",
    description="T-Rex Selector: High-Dimensional Variable Selection & FDR Control",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/ArnauVilella/TRexSelector-python",
    packages=find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.8",
    install_requires=[
        "numpy",
        "scipy",
        "scikit-learn",
        "tlars",
        "joblib",
        "matplotlib",
        "pandas"
    ],
)
