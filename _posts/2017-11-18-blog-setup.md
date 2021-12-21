---
layout: post
title: Blog setup
comments: true
---




The installation steps will be based on Max OS X operation system. I found following links usefull:
+ [Creating your Jekyll-Bootstrap powered blog for R blogging ](http://lcolladotor.github.io/2013/11/09/new-Fellgernon-Bit-setup-in-Github/#.WhC2VLQ-fUL).
+ [Systematic Investor Blog Setup](http://systematicinvestor.github.io/Steps).
+ [Install Jekyll on Mac OS X](https://learn.cloudcannon.com/jekyll/install-jekyll-on-os-x/).
+ [Lanyon](http://lanyon.getpoole.com/).
+ [Google Analytics](http://joshualande.com/jekyll-github-pages-poole).

  Below, I will outline steps I used to setup my blog.
 + Intall XCode


{% highlight r %}
xcode-select --install
{% endhighlight %}

 + Agree to license


{% highlight r %}
sudo xcodebuild -license
{% endhighlight %}

 + Install Homebrew


{% highlight r %}
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
{% endhighlight %}

 + Install Ruby


{% highlight r %}
brew install ruby
{% endhighlight %}

 + Install Jekyll


{% highlight r %}
sudo gem install jekyll
{% endhighlight %}

Now, we are ready to set the GitHub. One can use [GitHub Desktop] (https://desktop.github.com/) or do it manually. Let's focus on later.



{% highlight r %}
git clone https://github.com/poole/lanyon.git sysresearcher.github.io
cd sysresearcher.github.io
git remote set-url origin git@github.com:sysresearcher/sysresearcher.github.io.git
git push -u origin master
{% endhighlight %}

To update the respository, one can do


{% highlight r %}
git add .
git push
git commit -m "new post"
git push origin master
{% endhighlight %}

To clone git respository to any local place


{% highlight r %}
git clone https://github.com/sysresearcher.github.io.git sysresearcher.github.io
{% endhighlight %}


*(this report was produced on: 2017-12-09)*
