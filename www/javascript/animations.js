/**
 *
 * @author: Nicolas Petton <nicolas@petton.fr>
 *
 * @licstart  The following is the entire license notice for the
 *  JavaScript code in this page.
 *
 * Copyright (C) 2015 Free Software Foundation, Inc.
 *
 *
 * The JavaScript code in this page is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in this page.
 *
 */

(function() {
	function scrollTo(top, complete) {
		var body = $("html, body");
		body.stop().animate({scrollTop: top}, 400, 'swing', complete);
	}

	function scrollAfterHeader() {
		var top = $('.header').height();
		scrollTo(top);
	}

	function scrollToAnchor(name, complete) {
		var target = $(name);
		var top = target.offset().top;
		scrollTo(top, complete);
		history.pushState({}, name, name);
	}

	function setupAnchors() {
		$('a').click(function(evt) {
			var anchor = $(evt.target).closest('a');
			var href = anchor.attr('href');
			if(href[0] === '#') {
				scrollToAnchor(href, function() {
					window.location = href;
				});
				evt.preventDefault();
			}
		});
	}

	function setupMobileNav() {
		$('.nav-toggle').click(function() {
			$('nav.main').toggle();
		});
	}

	$(function() {
		setupAnchors();
		setupMobileNav();
	});
})();
