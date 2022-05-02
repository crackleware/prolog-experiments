var session;

var plconsult = window.plc = async (program) => {
	try {
		await session.promiseConsult(program);
	} catch (e) {
		console.log('catched:', session.format_answer(e));
	}
};

var plquery = window.plq = async (query) => {
	try {
		await session.promiseQuery(query);
		session.answers(x => console.log(session.format_answer(x)));
		// for await (let answer of session.promiseAnswers()) {
		// 	console.log(session.format_answer(answer));
		// }
	} catch (e) {
		console.log('catched:', session.format_answer(e));
	}
};

async function onload() {
	session = window.session = pl.create();

	var program = await (await fetch('app.pl')).text();
	await plconsult(program);

	const tstart = Date.now();
	await plquery("run.");
	console.log('execution duration: ', (Date.now() - tstart), 'ms');
	// await plquery("statistics.");
}
