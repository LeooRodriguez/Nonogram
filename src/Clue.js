import React from 'react';

class Clue extends React.Component {
    render() {
        const {clue} = this.props;
        const {satisface} = this.props;
        return (
            <div className={"clue" + (satisface ? " sat" : " nsat")} >
                {clue.map((num, i) =>
                    <div key={i}>
                        {num}
                    </div>
                )}
            </div>
        );
    }
}

export default Clue;